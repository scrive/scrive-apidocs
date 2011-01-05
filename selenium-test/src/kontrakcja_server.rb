require "rubygems"
gem "rspec"
gem "selenium-client"
require "selenium/client"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "fileutils"
require "popen4"

class KontrakcjaServer

  def start
    puts "Starting Kontrakcja Server ..."
    move_working_dir

    if File.directory? "_local" then
      FileUtils.rm_rf "_local"
    end

    Thread.new do
      cmd = "dist/build/kontrakcja-server/kontrakcja-server"
      POpen4::popen4 cmd do |stdout, stderr, stdin, pid|
        @pid = pid
      end
    end
    sleep 7

    reset_working_dir
  end

  def stop
    puts "Stopping Kontrakcja Server ..."
    Process.kill 15, @pid
  end

  private

  def move_working_dir
    if not Dir.getwd.end_with? "selenium-test" then
      raise "unexpected working directory, please run from selenium-test"
    end
    Dir.chdir ".."
    if not Dir.getwd.end_with? "kontrakcja" then
      raise "unexpected working directory"
    end
  end

  def reset_working_dir
    if Dir.getwd.end_with? "kontrakcja" then
      Dir.chdir "selenium-test"
    end
    if not Dir.getwd.end_with? "selenium-test" then
      raise "unexpected working directory"
    end
  end

end
