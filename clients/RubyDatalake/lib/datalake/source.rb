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
require 'json'
require 'net/http'
require 'sequel'
require 'recursive-open-struct'
require 'daru'

# Prevent Sequel from upcasing identifiers like table names
Sequel.identifier_output_method = nil
Sequel.identifier_input_method = nil

class Datalake::Source < RecursiveOpenStruct
  DB = Sequel::Database.new
  def initialize(attrs, dlake)
    super(attrs, recurse_over_arrays: true)
    @dlake = dlake
  end

  def get_contents
    req = Net::HTTP::Get.new("/source/#{ix}/download")
    res = @dlake.dlserver.request_with_auth req
    res.body
  end

  def to_df(&block)
    view_name = self.viewName
    query = DB.from(view_name)
    query = yield(query) if block_given?
    req = Net::HTTP::Post.new("/sql")
    req.add_field("Content-Type", "application/json")
    req.body = {sql: query.sql}.to_json
    resp = @dlake.dlserver.request_with_auth req
    arr = JSON.parse resp.body
    raise "Bad return code (#{resp.code}),\n reason: #{JSON.pretty_generate arr}" unless resp.kind_of? Net::HTTPOK
    Daru::DataFrame.new arr
  end
end
