require "rubygems"
require "fileutils"
require "popen4"

class KontrakcjaServer

  def start
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
