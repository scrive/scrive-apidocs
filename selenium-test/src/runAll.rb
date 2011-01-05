require "rubygems"
require "src/kontrakcja_server.rb"
require "src/selenium_server.rb"

@selenium_server = SeleniumServer.new
@selenium_server.start

@kontrakcja_server = KontrakcjaServer.new
@kontrakcja_server.start

if not Dir.getwd.end_with? "selenium-test" then
  raise "unexpected working directory, please run from selenium-test"
end

exec "spec --colour --format specdoc src/*_spec.rb"

@kontrakcja_server.stop
@selenium_server.stop
