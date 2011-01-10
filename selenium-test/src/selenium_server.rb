require "rubygems"
require "fileutils"
require "popen4"

class SeleniumServer

  def start
    puts "Starting Selenium Server ..."
    check_working_dir
    Thread.new do
      cmd = "java -jar lib/selenium-server-standalone-2.0a7.jar"
      POpen4::popen4 cmd do |stdout, stderr, stdin, pid|
        @pid = pid
      end
    end
    sleep 7
  end

  def stop
    puts "Stopping Selenium Server ..."
    check_working_dir
    Process.kill 15, @pid
  end

  private

  def check_working_dir
    if not Dir.getwd.end_with? "selenium-test" then
      raise "unexpected working directory, please run from selenium-test"
    end
  end

end
