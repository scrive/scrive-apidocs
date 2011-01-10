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

describe "password reminder page" do
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

  it "has login form in header when nobody is logged in" do
    UserHelper.is_login_form_present_in_header(page).should be_true
  end

  it "has logout link in header when somebody is logged in" do
    UserHelper.login_as_new_user(page, "oscar@corp.xyz", "Oscar O'Keefe", "passwordo")
    begin
      UserHelper.is_logout_link_present_in_header(page).should be_true
    ensure
      UserHelper.logout(page)
    end
  end

  it "sends an email with a link after a successful request" do
    UserHelper.create_new_user(page, "petri@corp.xyz", "Petri Pesky", "passwordp")
    UserHelper.request_password_reminder(page, "petri@corp.xyz")
    EmailHelper.is_email_for("petri@corp.xyz").should be_true
    EmailHelper.is_link_in_mail_for("petri@corp.xyz").should be_true
  end

  it "displays the login form after a successful request" do
    UserHelper.create_new_user(page, "quentin@corp.xyz", "Quentin Queue", "passwordq")
    UserHelper.request_password_reminder(page, "quentin@corp.xyz")
    UserHelper.is_login_form_present_in_main(page).should be_true
  end

  it "doesn't send an email if the account is non-existant" do
    UserHelper.request_password_reminder(page, "an_incorrect_email@xyz.xyz")
    UserHelper.is_email_for("an_incorrect_email@xyz.xyz").should be_false 
  end

  it "doesn't send an email if the account hasn't been activated" do
    UserHelper.register(page, "roberta@corp.xyz")
    UserHelper.request_password_reminder(page, "roberta@corp.xyz")
    UserHelper.is_email_for("roberta@corp.xyz").should be_false
  end

  it "displays reminder form again if email is left blank" do
    UserHelper.request_password_reminder(page, "")
    UserHelper.is_password_remind_form_present(page).should be_true 
  end

  it "has all footer links" do
    FooterHelper.are_footer_links_present(page).should be_true
  end
end
