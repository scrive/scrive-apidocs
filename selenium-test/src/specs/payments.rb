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

  it "new user can pay with credit card on outside" do
    random_email = rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + "-test@mailinator.com"
    puts "using random email : " + random_email

    puts "go to price plan page"
    @h.driver.get(@h.ctx.createKontrakcjaURL "/en/pricing")
    
    puts "fill in a name"
    (@h.wait.until { @h.driver.find_element :css => ".first_name input" }).send_keys "Random"
    (@h.wait.until { @h.driver.find_element :css => ".last_name input" }).send_keys "Person"
    (@h.wait.until { @h.driver.find_element :css => ".email input" }).send_keys random_email

    (@h.wait.until { @h.driver.find_element :css => ".card_number input" }).send_keys "4111 1111 1111 1111"
    sel =(@h.wait.until { @h.driver.find_element :css => ".field.expires .year select" })

    sel.find_elements( :css => ".year select option" ).find do |option|
      option.text == "20"
    end.click

    (@h.wait.until { @h.driver.find_element :css => ".field.cvv input" }).send_keys "111"

    (@h.wait.until { @h.driver.find_element :css => ".s-subscribe" }).click

    (@h.wait.until { @h.driver.find_element :css => ".modal-footer a .label"}).click
    (@h.wait.until { @h.driver.find_element :css => ".modal-footer a.login-button"})

  end

  it "new user can pay with credit card on outside after typing stuff in wrong" do
    random_email = rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + "-test@mailinator.com"
    puts "using random email : " + random_email

    puts "go to price plan page"
    @h.driver.get(@h.ctx.createKontrakcjaURL "/en/pricing")
    
    puts "fill in a name"
    (@h.wait.until { @h.driver.find_element :css => ".first_name input" }).send_keys "Random"
    (@h.wait.until { @h.driver.find_element :css => ".last_name input" }).send_keys "Person"
    (@h.wait.until { @h.driver.find_element :css => ".email input" }).send_keys random_email

    (@h.wait.until { @h.driver.find_element :css => ".card_number input" }).send_keys "4111 1111 1111 1110"
    sel =(@h.wait.until { @h.driver.find_element :css => ".field.expires .year select" })

    sel.find_elements( :css => "option" ).find do |option|
      option.text == "20"
    end.click

    (@h.wait.until { @h.driver.find_element :css => ".field.cvv input" }).send_keys "111"

    (@h.wait.until { @h.driver.find_element :css => ".s-subscribe" }).click

    (@h.wait.until { @h.driver.find_element :css => ".card_number input" }).send_keys "\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83"
    (@h.wait.until { @h.driver.find_element :css => ".card_number input" }).send_keys "4111 1111 1111 1111"

    (@h.wait.until { @h.driver.find_element :css => ".s-subscribe" }).click

    (@h.wait.until { @h.driver.find_element :css => ".modal-footer a .label"}).click
    (@h.wait.until { @h.driver.find_element :css => ".modal-footer a.login-button"})

  end

  it "new user can pay on the inside with popup" do
    random_email = rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + "-test@mailinator.com"
    puts "using random email : " + random_email

    @h.driver.get(@h.ctx.createKontrakcjaURL "/")

    puts "request an account and make sure you get a green flash back"
    (@h.wait.until { @h.driver.find_element :css => ".requestAccount input" }).send_keys random_email
    (@h.wait.until { @h.driver.find_element :css => ".requestAccount .submit" }).click
    (@h.wait.until { @h.driver.find_element :css => ".modal-container .close" }).click

    puts "we should get an email to a page where we can accept the tos"
    @h.emailhelper.follow_link_in_latest_mail_for random_email
    @h.wait.until { @h.driver.find_element :id => "tosCBox" }

    puts "accept the tos"
    (@h.wait.until { @h.driver.find_element :id => "tosCBox" }).click

    puts "fill in a name"
    (@h.wait.until { @h.driver.find_element :name => "fstname" }).send_keys "Random"
    (@h.wait.until { @h.driver.find_element :name => "sndname" }).send_keys "Person"

    puts "fill in the password details correctly"
    (@h.wait.until { @h.driver.find_element :name => "password" }).send_keys "password-123"
    (@h.wait.until { @h.driver.find_element :name => "password2" }).send_keys "password-123"

    puts "submit the signup form"
    (@h.wait.until { @h.driver.find_element :css => ".modal-footer .btn-small.float-right" }).click

    puts "should be logged in and able to upload a document"
    @h.wait.until { @h.driver.find_element :css => "a.logout" }
    @h.driver.get(@h.ctx.createKontrakcjaURL "/d")
    @h.wait.until { @h.driver.find_element :css => ".archive" }

    (@h.wait.until { @h.driver.find_element :css => ".blocking-info" }).click
    

    (@h.wait.until { @h.driver.find_element :css => ".card_number input" }).send_keys "4111 1111 1111 1111"
    sel =(@h.wait.until { @h.driver.find_element :css => ".field.expires .year select" })

    sel.find_elements( :css => "option" ).find do |option|
      option.text == "20"
    end.click

    (@h.wait.until { @h.driver.find_element :css => ".field.cvv input" }).send_keys "111"

    (@h.wait.until { @h.driver.find_element :css => ".s-subscribe" }).click

    @h.wait.until { (@h.driver.find_element :css => ".blocking-info").text == ""}

  end

  it "new user can pay on the inside with subscription tab" do
    random_email = rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + "-test@mailinator.com"
    puts "using random email : " + random_email

    @h.driver.get(@h.ctx.createKontrakcjaURL "/")

    puts "request an account and make sure you get a green flash back"
    (@h.wait.until { @h.driver.find_element :css => ".requestAccount input" }).send_keys random_email
    (@h.wait.until { @h.driver.find_element :css => ".requestAccount .submit" }).click
    (@h.wait.until { @h.driver.find_element :css => ".modal-container .close" }).click

    puts "we should get an email to a page where we can accept the tos"
    @h.emailhelper.follow_link_in_latest_mail_for random_email
    @h.wait.until { @h.driver.find_element :id => "tosCBox" }

    puts "accept the tos"
    (@h.wait.until { @h.driver.find_element :id => "tosCBox" }).click

    puts "fill in a name"
    (@h.wait.until { @h.driver.find_element :name => "fstname" }).send_keys "Random"
    (@h.wait.until { @h.driver.find_element :name => "sndname" }).send_keys "Person"

    puts "fill in the password details correctly"
    (@h.wait.until { @h.driver.find_element :name => "password" }).send_keys "password-123"
    (@h.wait.until { @h.driver.find_element :name => "password2" }).send_keys "password-123"

    puts "submit the signup form"
    (@h.wait.until { @h.driver.find_element :css => ".modal-footer .btn-small.float-right" }).click

    puts "should be logged in and able to upload a document"
    @h.wait.until { @h.driver.find_element :css => "a.logout" }
    @h.driver.get(@h.ctx.createKontrakcjaURL "/account")

    
    (@h.wait.until { @h.driver.find_element :css => ".s-subscription" }).click
    

    (@h.wait.until { @h.driver.find_element :css => ".card_number input" }).send_keys "4111 1111 1111 1111"
    sel =(@h.wait.until { @h.driver.find_element :css => ".field.expires .year select" })

    sel.find_elements( :css => "option" ).find do |option|
      option.text == "20"
    end.click

    (@h.wait.until { @h.driver.find_element :css => ".field.cvv input" }).send_keys "111"

    (@h.wait.until { @h.driver.find_element :css => ".s-subscribe" }).click

    @h.wait.until { @h.driver.find_element :css => ".changebilling-form"}

  end

  #  it "allows a user to pay with a credit card" do

#    @h.loginhelper.login_as(@h.ctx.props.tester_email, @h.ctx.props.tester_password)
#    begin
#      @h.loginhelper.set_name(@h.ctx.props.tester_fstname, @h.ctx.props.tester_sndname)
#
#      (@h.wait.until { @h.driver.find_element :css => ".s-account" }).click
#      (@h.wait.until { @h.driver.find_element :css => ".s-subscription" }).click
#
#      (@h.wait.until { @h.driver.find_element :css => ".field.card_number input" }).send_keys "4111111111111111"
#      sel =(@h.wait.until { @h.driver.find_element :css => ".field.expires .year select" })
#      sel.click
#      sel.find_elements( :tag_name => "option" ).find do |option|
#        option.text == "20"
#      end.click
#
#      (@h.wait.until { @h.driver.find_element :css => ".field.cvv input" }).send_keys "111"
#
#      (@h.wait.until { @h.driver.find_element :css => ".s-subscribe" }).click
#      (@h.wait.until { @h.driver.find_element :css => ".payments"    }).send_keys [:control, :home]
#      
#      @h.wait.until { @h.driver.find_element :css => ".subscription-payments" }
#    end
#  end
end
