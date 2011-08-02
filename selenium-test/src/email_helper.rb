require "rubygems"
gem "rspec"
gem "selenium-client"
require "selenium/client"
require "selenium/rspec/spec_helper"
require "spec/test/unit"

module EmailHelper

  def get_link_in_latest_mail_for email
    path = get_latest_mail_for email
    file = File.new(path, "r")
    file.each_line do |line|
      match = find_link_in line
      if match!=nil then
        return match
      end
    end
    return nil
  end
  
  def get_latest_mail_for email
    all_emails = Dir.glob(Dir.tmpdir + "/Email-" + email + "*.eml")
    all_emails.max { |a,b| File.mtime(a)<=>File.mtime(b) }
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
