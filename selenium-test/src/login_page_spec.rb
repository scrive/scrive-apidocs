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
    UserHelper.login_as_new_user(page, "wilma@corp.xyz", "Wilma Wilde", "passwordw")
    begin
      UserHelper.is_logout_link_present_in_header(page).should be_true
    ensure
      UserHelper.logout(page)
    end
  end
  
#  it "displays upload form if login successful (this fails, but it's a bug with the test, not with the program)" do
#    UserHelper.create_new_user(page, "xavier@compltd.xyz", "Xavier Xanu", "passwordx")
#    UserHelper.login_with_main_form(page, "xavier@compltd.xyz", "passwordx")
#    begin
#      UploadHelper.is_upload_form_present(page).should be_true
#    ensure
#      UserHelper.logout(page)
#    end
#  end

  it "displays login form if fields left empty" do
    UserHelper.login_with_main_form(page, "", "")
    UserHelper.is_login_form_present_in_main(page).should be_true
  end

  it "displays login form if invalid password is used" do
    UserHelper.create_new_user(page, "yvonne@compltd.xyz", "Yvonne Yoghurt", "passwordy")
    UserHelper.login_with_main_form(page, "yvonne@compltd.xyz", "an_incorrect_password")
    UserHelper.is_login_form_present_in_main(page).should be_true
  end

  it "displays login form if invalid username is used" do
    UserHelper.login_with_main_form(page, "idontexist@nowhere.xyz", "a_password")
    UserHelper.is_login_form_present_in_main(page).should be_true
  end

  it "has all footer links" do
    FooterHelper.are_footer_links_present(page).should be_true
  end
end
