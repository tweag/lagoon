Gem::Specification.new do |s|
  s.name               = "datalake"
  s.version            = "0.0.2"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Nicolas Mattia"]
  s.date = %q{2016-11-22}
  s.description = %q{Ruby client for Pfizer Datalake project}
  s.email = %q{nicolas.mattia@tweag.io}
  s.files = [ "lib/datalake.rb",
              "lib/datalake/source.rb",
              "lib/datalake/helper.rb"]

  s.add_runtime_dependency 'recursive-open-struct'
  s.add_runtime_dependency 'daru'
  s.add_runtime_dependency 'sequel', '< 5'
  s.add_runtime_dependency 'json'

  s.require_paths = ["lib"]
  s.rubygems_version = %q{1.6.2}
  s.summary = %q{Datalake gem}

  if s.respond_to? :specification_version then
    s.specification_version = 3

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
    else
    end
  else
  end
end
