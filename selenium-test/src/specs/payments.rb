require "rubygems"
gem "rspec"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "selenium-test/src/helpers.rb"

describe "subscribe with a credit card" do

  before(:each) do
    @h = Helpers.new
  end

  append_after(:each) do
    @h.quit
  end

  it "allows a user to pay with a credit card" do

    @h.loginhelper.login_as(@h.ctx.props.tester_email, @h.ctx.props.tester_password)
    begin
      @h.loginhelper.set_name(@h.ctx.props.tester_fstname, @h.ctx.props.tester_sndname)

      (@h.wait.until { @h.driver.find_element :css => ".s-account" }).click
      (@h.wait.until { @h.driver.find_element :css => ".s-subscription" }).click

      (@h.wait.until { @h.driver.find_element :css => ".field.card_number input" }).send_keys "4111111111111111"
      (@h.wait.until { @h.driver.find_element :css => ".field.expires .year select" }).select 20
      
      (@h.wait.until { @h.driver.find_element :css => ".field.cvv input" }).send_keys "111"

      (@h.wait.until { @h.driver.find_element :css => ".s-subscribe" }).click

      @h.wait.until { @h.driver.find_element :css => ".subscription-payments" }
    end
  end
end
