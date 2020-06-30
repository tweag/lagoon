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
