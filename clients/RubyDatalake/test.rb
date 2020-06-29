#!/usr/bin/ruby
#
# Run the RubyDatalake tests
#
# Variables (test specific and datalake related):
#   RUBY_TEST_DIR : directory containing 'gene_with_protein_product_small.json'
#                   and 'simple.csv'
#   USER          : name of the test user
#   PASSWORD      : password of the test user
#   DATALAKE_HOST : datalake-server host name
#   DATALAKE_PORT : datalake-server port

require 'datalake'
require 'csv'

def some_src_content
  <<-EOF
foo,bar
1,2
  EOF
end

# We use a stripped down version of
# http://ftp.ebi.ac.uk/pub/databases/genenames/new/json/locus_types/gene_with_protein_product.json
# with only 29 rows.
N_GENE_PROTEIN_ROWS = 29
GENE_PROTEIN_NAME   = 'gene_with_protein_product.json'
RUBY_TEST_DIR       = ENV['RUBY_TEST_DIR'] || 'test-cases'
GENE_PROTEIN_FILE   = File.join(RUBY_TEST_DIR, 'gene_with_protein_product_small.json')
CSV_FILE            = File.join(RUBY_TEST_DIR, 'simple.csv')

dlake = Datalake.new(verbose: true)

# Raise an error if the DataFrame's number of rows is not 'n_rows'
def assert_n_rows(df, n_rows)
  raise "Bad number of rows, got #{df.nrows} instead of #{n_rows}" if df.nrows != n_rows
end

# Raise an error if the source's content is not the expected content. The
# source's content is parsed as a CSV.
def assert_csv_content(src, src_content)
  actual = CSV.parse(src.get_contents)
  if src_content != actual
    raise "Bad content for source #{src.name},\n" \
          "Expected:\n #{src_content}\n" \
          "Actual:\n #{actual}"
  end
end

10.times do |i|
  some_src = dlake.ingest(CSV_FILE, name: "src-#{i}", quiet: true)
  assert_csv_content(some_src, CSV.parse(some_src_content))
end

# Before we load, there should be no sources
raise "Sources should be nil" unless dlake.sources.nil?
dlake.load

raise "Sources should be loaded" if dlake.sources.nil?

attrs = { json_path: '{ "response" : { "docs" : [_] } }',
          name: GENE_PROTEIN_NAME ,
          quiet: true }

gene_protein_src = dlake.ingest(GENE_PROTEIN_FILE, attrs)

# Perform some tests on the resulting source
raise "Gene protein should have columns" if gene_protein_src.columns.empty?

df = gene_protein_src.to_df
assert_n_rows(df, N_GENE_PROTEIN_ROWS)
df = gene_protein_src.to_df {|x| x.filter('ix > 10').filter('ix <= 25')}
assert_n_rows(df, 15)

dlake.load

# Perform the tests on the same source, this time loaded through
# datalake-server
gene_protein_src = dlake.sources.find {|src| src.name == GENE_PROTEIN_NAME}
df = gene_protein_src.to_df
assert_n_rows(df, N_GENE_PROTEIN_ROWS)
df = gene_protein_src.to_df {|x| x.filter('ix > 10').filter('ix <= 25')}
assert_n_rows(df, 15)

dlake.load(1..5)
raise "Bad range for load" unless dlake.sources.length == 5

dlake.load(1...5)
raise "Bad range for load" unless dlake.sources.length == 4
