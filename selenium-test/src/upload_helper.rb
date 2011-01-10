require "rubygems"
gem "selenium-client"
require "selenium/client"

module UploadHelper

  def is_upload_form_present(page)
    page.is_element_present("//form[@action='/d']")
  end

end
