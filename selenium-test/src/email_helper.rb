require "rubygems"
gem "rspec"
require "selenium-webdriver"

class EmailHelper

  def initialize(ctx, driver, helper)
    @driver = driver
    @ctx = ctx
    @h = helper

    @loginhelper = LoginHelper.new(@ctx, @driver, @h)
  end

  def random_email(random_chars=10)
    email = ""
    for i in 0..random_chars
      email += rand(10).to_s
    end
    email += "-test@mailinator.com"
    puts "using random email : " + email
    return email
  end

  def follow_link_in_latest_mail_for email

    link = ""
    @loginhelper.login_as(@ctx.props.tester_email, @ctx.props.tester_password)
    begin
      @driver.navigate().to(@ctx.createKontrakcjaURL("/dave/backdoor/" + email))
      @h.wait_until { @driver.find_element :css => ".bodyContent a" }
      link = (@driver.find_elements :css => ".bodyContent a").first.attribute("href")
      puts "Link from latest email to " + email + " has link " + link
    ensure
      @loginhelper.logout
    end
    @driver.navigate().to(link)
  end
end
