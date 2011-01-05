require "rubygems"
gem "rspec"
gem "selenium-client"
require "selenium/client"
require "selenium/rspec/spec_helper"
require "spec/test/unit"

module UploadHelper

  def is_upload_form_present(page)
    page.is_element_present("//form[@action='/d']")
  end

end
