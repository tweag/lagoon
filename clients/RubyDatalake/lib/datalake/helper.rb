require 'cgi'
require 'datalake'

class String
  def camel_case_lower
    self.split('_').inject([]){ |buffer,e| buffer.push(buffer.empty? ? e : e.capitalize) }.join
  end
end

class Datalake::Helper
  def self.to_request_params params
    tags = params.delete(:tags) || []
    tag_params = tags.map{|t| "tag=#{CGI::escape(t)}"}
    columns = params.delete(:columns) || []
    col_params = columns.map{|t| "column=#{CGI::escape(t)}"}
    key_val_params = params.map{|k,v| "#{k.to_s.camel_case_lower}=#{map_param v}"}
    (key_val_params+tag_params+col_params).join('&')
  end

  def self.map_param param
    param = param.strftime("%Y-%m-%d %H:%M:%S") if param.kind_of? Time
    param = param.to_s
    param = CGI::escape(param)
    param
  end
end
