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
