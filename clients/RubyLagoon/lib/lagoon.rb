# Copyright 2020 Pfizer Inc.

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

#     https://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
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

class Lagoon
  attr_reader :sources
  attr_reader :lagoonserver

  def initialize (attrs = {})

    lagoonserver_attrs = attrs[:lagoonserver] || {}
    yaml_config_path = attrs[:file]

    if yaml_config_path
      yaml_config = YAML.load_file(yaml_config_path)
      lagoonserver_host_yaml     = yaml_config["lagoonserver_host"]
      lagoonserver_port_yaml     = yaml_config["lagoonserver_port"]
      lagoonserver_user_yaml     = yaml_config["user"]
      lagoonserver_password_yaml = yaml_config["password"]
      yaml_config_verbose    = yaml_config["verbose"]
    end

    lagoonserver_host = lagoonserver_attrs[:host]     || lagoonserver_host_yaml     || ENV["LAGOON_HOST"]
    lagoonserver_port = lagoonserver_attrs[:port]     || lagoonserver_port_yaml     || ENV["LAGOON_PORT"]
    user          = lagoonserver_attrs[:user]     || lagoonserver_user_yaml     || ENV["USER"]
    password      = lagoonserver_attrs[:password] || lagoonserver_password_yaml || ENV["PASSWORD"]
    @verbose      = attrs[:verbose]           || yaml_config_verbose    || ENV["LAGOON_RUBY_VERBOSE"]

    authenticate = true
    if attrs[:authenticate] == false
      authenticate = false
    end

    raise ArgumentError, "No host for lagoon-server" if lagoonserver_host.nil?
    raise ArgumentError, "No port for lagoon-server" if lagoonserver_port.nil?

    @lagoonserver = Net::HTTP.new(lagoonserver_host, lagoonserver_port)
    if lagoonserver_attrs[:protocol].upcase == 'HTTPS'
       @lagoonserver.use_ssl=true
       pem = File.read( ENV['SSL_CERT_FILE'])
       @lagoonserver.ca_file=pem
       @lagoonserver.key = OpenSSL::PKey::RSA.new 2048
       @lagoonserver.verify_mode = OpenSSL::SSL::VERIFY_NONE
    end
    @lagoonserver.extend(Auth)
    if (user && password)
      if authenticate
        log_info "Found credentials, authenticating"
        req = Net::HTTP::Post.new("/user/login")
        req.add_field("Content-Type", "application/json")
        req.body = {user: user, pass: password}.to_json
        resp = @lagoonserver.request req
        case resp
        when Net::HTTPOK
          @lagoonserver.token = resp["Set-Cookie"]
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

  # Load the Lagoon object with sources. A range can be specified, which
  # will be translated into `offset` and `limit` parameters.
  def load(rng = nil, attrs = {})

    params = {}
    params[:offset] = rng.begin unless rng.nil?
    params[:limit] =
      rng.end - rng.begin + (rng.exclude_end? ? 0 : 1) unless rng.nil?

    params.merge! attrs
    uri_params = Helper.to_request_params params
    req = Net::HTTP::Get.new("/sources?"+uri_params)
    res = @lagoonserver.request_with_auth req
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

    @lagoonserver.request_with_auth req do |resp|
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

require 'lagoon/helper'
require 'lagoon/source'
