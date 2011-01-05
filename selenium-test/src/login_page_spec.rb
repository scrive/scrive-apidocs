require "rubygems"
gem "rspec"
gem "selenium-client"
require "selenium/client"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "src/user_helper"
include UserHelper
require "src/footer_helper"
include FooterHelper
require "src/upload_helper"
include UploadHelper
require "src/kontrakcja_server.rb"

describe "login page" do
  attr_reader :selenium_driver
  alias :page :selenium_driver

  before(:all) do
    @verification_errors = []
    @selenium_driver = Selenium::Client::Driver.new \
      :host => "localhost",
      :port => 4444,
      :browser => "*chrome",
      :url => "http://localhost:8000/",
      :timeout_in_second => 60
    @selenium_driver.start_new_browser_session
  end

  before(:each) do
    page.open "/login"
    page.wait_for_page_to_load "30000"
  end
  
  append_after(:each) do
    @verification_errors.should == []
  end

  append_after(:all) do
    @selenium_driver.close_current_browser_session
  end

  it "has login form in header when nobody is logged in" do
    UserHelper.is_login_form_present_in_header(page).should be_true
  end

  it "has logout link in header when somebody is logged in" do
    UserHelper.register_login(page, "Bob Blue", "bob@corp.xyz", "passwordb")
    begin
      UserHelper.is_logout_link_present_in_header(page).should be_true
    ensure
      UserHelper.logout(page)
    end
  end
  
  it "has all footer links" do
    FooterHelper.are_footer_links_present(page).should be_true
  end

  it "displays login form if invalid password is used" do
    UserHelper.register_login_and_logout(page, "Clara Carridge", "clara@compltd.xyz", "passwordc")
    UserHelper.login(page, "clara@compltd.xyz", "an_incorrect_password")
    UserHelper.is_login_form_present_in_main(page).should be_true
  end

  it "displays login form if invalid username is used" do
    UserHelper.login(page, "idontexist@nowhere.xyz", "a_password")
    UserHelper.is_login_form_present_in_main(page).should be_true
  end

  it "displays upload form if login successful" do
    UserHelper.register_login_and_logout(page, "Laura Loughton", "laura@comp.xyz", "passwordl")
    begin
      UserHelper.login(page, "laura@comp.xyz", "passwordl")
      UploadHelper.is_upload_form_present(page).should be_true
    ensure
      UserHelper.logout(page)
    end
  end

end
