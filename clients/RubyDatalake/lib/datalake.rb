require 'net/http'
require 'json'
require 'yaml'

module Auth
  attr_accessor :token

  def request_with_auth(req, &block)
    req['Cookie'] = @token if @token
    self.request(req, &block)
  end
end

class Datalake
  attr_reader :sources
  attr_reader :dlserver

  def initialize (attrs = {})

    dlserver_attrs = attrs[:dlserver] || {}
    yaml_config_path = attrs[:file]

    if yaml_config_path
      yaml_config = YAML.load_file(yaml_config_path)
      dlserver_host_yaml     = yaml_config["dlserver_host"]
      dlserver_port_yaml     = yaml_config["dlserver_port"]
      dlserver_user_yaml     = yaml_config["user"]
      dlserver_password_yaml = yaml_config["password"]
      yaml_config_verbose    = yaml_config["verbose"]
    end

    dlserver_host = dlserver_attrs[:host]     || dlserver_host_yaml     || ENV["DATALAKE_HOST"]
    dlserver_port = dlserver_attrs[:port]     || dlserver_port_yaml     || ENV["DATALAKE_PORT"]
    user          = dlserver_attrs[:user]     || dlserver_user_yaml     || ENV["USER"]
    password      = dlserver_attrs[:password] || dlserver_password_yaml || ENV["PASSWORD"]
    @verbose      = attrs[:verbose]           || yaml_config_verbose    || ENV["DATALAKE_RUBY_VERBOSE"]

    authenticate = true
    if attrs[:authenticate] == false
      authenticate = false
    end

    raise ArgumentError, "No host for datalake-server" if dlserver_host.nil?
    raise ArgumentError, "No port for datalake-server" if dlserver_port.nil?

    @dlserver = Net::HTTP.new(dlserver_host, dlserver_port)
    if dlserver_attrs[:protocol].upcase == 'HTTPS'
       @dlserver.use_ssl=true
       pem = File.read( ENV['SSL_CERT_FILE'])
       @dlserver.ca_file=pem
       @dlserver.key = OpenSSL::PKey::RSA.new 2048
       @dlserver.verify_mode = OpenSSL::SSL::VERIFY_NONE
    end
    @dlserver.extend(Auth)
    if (user && password)
      if authenticate
        log_info "Found credentials, authenticating"
        req = Net::HTTP::Post.new("/user/login")
        req.add_field("Content-Type", "application/json")
        req.body = {user: user, pass: password}.to_json
        resp = @dlserver.request req
        case resp
        when Net::HTTPOK
          @dlserver.token = resp["Set-Cookie"]
          log_info "Authentication successful for user #{user}"
        else
          raise "Authentication failed for user #{user}, code: #{resp.code}"
        end
      else
        log_info "Not authenticating"
      end
    else
      log_warn "No credentials found, not authenticating"
    end
  end

  def log_info(str)
    if @verbose
      str.lines.each do |l|
        puts "[INFO] #{l}"
      end
    end
  end

  def log_warn(str)
    if @verbose
      str.lines.each do |l|
        puts "[WARN] #{l}"
      end
    end
  end

  # Load the Datalake object with sources. A range can be specified, which
  # will be translated into `offset` and `limit` parameters.
  def load(rng = nil, attrs = {})

    params = {}
    params[:offset] = rng.begin unless rng.nil?
    params[:limit] =
      rng.end - rng.begin + (rng.exclude_end? ? 0 : 1) unless rng.nil?

    params.merge! attrs
    uri_params = Helper.to_request_params params
    req = Net::HTTP::Get.new("/sources?"+uri_params)
    res = @dlserver.request_with_auth req
    json_sources = JSON.parse(res.body)
    @sources = json_sources.map { |src| Source.new(src, self)}
    return nil
  end

  # Ingest a file
  # returns a Source object (result of the ingest process)
  # TODO: describe parameter inference
  def ingest (file, attrs = {})
    file = File.new(file, "r") if file.kind_of? String

    source_filename = File.basename file
    attrs[:name] = attrs[:name] || File.basename(file, ".*")

    params = { input: source_filename }

    params.merge! attrs
    uri_params = Helper.to_request_params params

    # Create request object
    # TODO: This is not going to work with big files
    req = Net::HTTP::Post.new("/sources?"+uri_params)
    req['Content-Length'] = File.size file
    req.body_stream = File.open file

    json_source = nil

    @dlserver.request_with_auth req do |resp|
      # Deal with the response. Response comes  (mostly) in pairs:
      #   - {"start": <some action>}   <-- <some action> was started
      #   - "ok"                       <-- the last <some action> finished
      #
      # We keep track of the state using a stack. Since we're getting packets
      # from the socket we need to keep track of the leftover, since we're
      # parsing lines.
      leftover = ""
      state = []
      resp.read_body do |segment|
        leftover = leftover + segment
        lines = leftover.lines

        # 'lines' will keep the '\n' character. Whatever ends with '\n' was
        # indeed a full line. The rest (which should be a single element array)
        # is the leftover.
        readys_leftover = lines.partition {|l| l.end_with? "\n"}
        readys = readys_leftover[0].map(&:chomp)
        leftover = readys_leftover[1][0] || ""

        readys.each do |l|
          if l == "\"ok\""
            # In case of "ok" we pop the last state and log that it's now done.
            log_info "Done: #{state.pop}"
          else
            # In case of a "start" we log the new action and push it on the
            # stack.
            new_blob = JSON.parse(l)
            if  start_token = new_blob["start"]
              # In case of a "start" we log the new action and push it on the
              # stack.
              log_info "Start: " + start_token
              state.push start_token
            elsif notice_token = new_blob["notice"]
              # Sometimes it's a "notice"; very well, just log it.
              log_info "Notice: " + notice_token
            else
              # If it's neither "ok", a start or a notice, just log its JSON
              # representation and implicitly assume it might be the source
              # metadata.
              json_source = JSON.parse(l)
              log_info (JSON.pretty_generate json_source)
            end
          end
        end
      end
    end
    Source.new(json_source, self)
  end
end

require 'datalake/helper'
require 'datalake/source'