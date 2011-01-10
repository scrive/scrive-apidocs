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

describe "change password page" do
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
    page.open "/amnesia"
    page.wait_for_page_to_load "30000"
  end
  
  append_after(:each) do
    @verification_errors.should == []
  end

  append_after(:all) do
    @selenium_driver.close_current_browser_session
  end

  it "lets you login with new password when successful" do
    UserHelper.create_new_user(page, "sandy@comltd.xyz", "Sandy Singer", "passwords")
    UserHelper.request_password_reminder(page, "sandy@comltd.xyz")
    UserHelper.change_password(page, "sandy@comltd.xyz", "passwords_2", "passwords_2")
    UserHelper.login(page, "sandy@comltd.xyz", "passwords_2")
    UserHelper.logout(page)
  end

  it "displays change password form again if passwords are left empty" do
    UserHelper.create_new_user(page, "tom@comltd.xyz", "Tom Thumb", "passwordt")
    UserHelper.request_password_reminder(page, "tom@comltd.xyz")
    UserHelper.change_password(page, "tom@comltd.xyz", "", "")
    UserHelper.is_change_password_form_present(page).should be_true
  end

  it "displays change password form again if passwords don't match" do
    UserHelper.create_new_user(page, "ursula@comltd.xyz", "Ursula Ulla", "passwordu")
    UserHelper.request_password_reminder(page, "ursula@comltd.xyz")
    UserHelper.change_password(page, "ursula@comltd.xyz", "passwordu_2", "not_passwordu_2")
    UserHelper.is_change_password_form_present(page).should be_true
  end

  it "displays change password form again if password shorter than 6 chars" do
    UserHelper.create_new_user(page, "vince@comltd.xyz", "Vincent Violet", "passwordv")
    UserHelper.request_password_reminder(page, "vince@comltd.xyz")
    UserHelper.change_password(page, "vince@comltd.xyz", "just5", "just5")
    UserHelper.is_change_password_form_present(page).should be_true
  end
end
