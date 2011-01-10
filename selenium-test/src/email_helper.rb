require "rubygems"
gem "rspec"
gem "selenium-client"
require "selenium/client"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "fileutils" 

module EmailHelper

  def clear_all
    FileUtils.rm Dir.glob(Dir.tmpdir + "/Email-*.eml"), :force => true
  end

  def is_email_for(address)
    path = get_mail_path(address)
    return File.file? path  
  end

  def is_link_in_mail_for(address)
    match = get_link_in_mail_for address
    return match!=nil
  end

  def get_link_in_mail_for(address)
    path = get_mail_path(address)
    file = File.new(path, "r")
    file.each_line do |line|
      match = find_link_in line
      if match!=nil then
        return match
      end
    end
    return nil
  end

  private

  def get_mail_path(for_email)
    return Dir.tmpdir + "/Email-" + for_email + ".eml"
  end

  def find_link_in(line)
    matches = line.match('<a href=\"(.*)\"')
    if matches!=nil && matches.length>1 then
      return matches[1]
    else
      return nil
    end
  end
end
