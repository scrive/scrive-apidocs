# -*- coding: utf-8 -*-
require "rubygems"
gem "rspec"
require "selenium-webdriver"

EMAIL_TITLES = {"invitation to sign"=> {"en"=> "Document to e-sign: contract",
                                        "sv"=> "Dokument att e-signera: contract",
                                        "fi"=> "E-allekirjoitettava dokumentti: contract",
                                        "it"=> "Documento da firmare elettronicamente: contract",
                                        "no"=> "Dokument til å e-signere: contract",
                                        "fr"=> "Document à e-signer: contract",
                                        "da"=> "Dokument, der skal e-signeres: contract",
                                        "es"=> "Documento a firmar electrónicamente: contract",
                                        "el"=> "Έγγραφο προς ηλεκτρονική υπογραφή: contract",
                                        "pt"=> "Documento a e-assinar: contract",
                                        "de"=> "Zum E-Signieren vorgelegtes Dokument: contract",
                                        "nl"=> "Document om te e-ondertekenen: contract"},
                "new password"=> {"en"=> "New password",
                                  "sv"=> "Nytt lösenord",
                                  "fi"=> "Uusi salasana",
                                  "it"=> "Nuova password",
                                  "no"=> "Nytt passord",
                                  "fr"=> "Nouveau mot de passe",
                                  "da"=> "Ny adgangskode",
                                  "es"=> "Nueva contraseña",
                                  "el"=> "Νέος κωδικός πρόσβασης",
                                  "pt"=> "Nova palavra-passe",
                                  "de"=> "Neues Passwort",
                                  "nl"=> "Nieuw wachtwoord"},
                "activate account"=> {"en"=> "Please activate your account",
                                      "sv"=> "Vänligen aktivera ditt konto",
                                      "fi"=> "Aktivoi tilisi",
                                      "it"=> "Ti preghiamo di attivare l'account",
                                      "no"=> "Vennligst aktiver din konto",
                                      "fr"=> "Veuillez activer votre compte",
                                      "da"=> "Aktiver venligst din konto",
                                      "es"=> "Por favor active su cuenta",
                                      "el"=> "Παρακαλώ ενεργοποιήστε το λογαριασμό σας",
                                      "pt"=> "Active a conta",
                                      "de"=> "Bitte aktivieren Sie Ihr Konto",
                                      "nl"=> "Activeer a.u.b. uw account"},
                "change username"=> {"en"=> "Change of username",
                                     "sv"=> "Ändring av användarnamn",
                                     "fi"=> "Käyttäjätunnuksen muuttaminen",
                                     "it"=> "Modifica del nome utente",
                                     "no"=> "Endre brukernavn",
                                     "fr"=> "Modification du nom d'utilisateur",
                                     "da"=> "Ændring af brugernavn",
                                     "es"=> "Cambio de nombre de usuario",
                                     "el"=> "Αλλαγή κωδικού χρήστη",
                                     "pt"=> "Alteração do nome de utilizador",
                                     "de"=> "Änderung des Benutzernamens",
                                     "nl"=> "Verandering van gebruikersnaam"},
                "document was saved"=> {"en"=> "A document was saved in your e-archive",
                                        "sv"=> "Ett dokument har sparats i ditt e-arkiv",
                                        "fi"=> "Dokumentti talletettiin e-arkistoosi",
                                        "it"=> "È stato salvato un documento nel tuo archivio elettronico",
                                        "no"=> "Et dokument ble lagret i ditt e-arkiv",
                                        "fr"=> "Un document a été sauvegardé dans vos e-archives",
                                        "da"=> "Et dokument blev gemt i dit e-arkiv",
                                        "es"=> "Un documento fue guardado en su archivo electrónico",
                                        "el"=> "Ένα έγγραφο αποθηκεύτηκε στο ηλεκτρονικό σας αρχείο",
                                        "pt"=> "Foi guardado um documento no seu arquivo electrónico",
                                        "de"=> "Ein Dokument wurde in Ihrem E-Archiv gespeichert",
                                        "nl"=> "Een document is opgeslagen in uw e-archief"}}

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

  def follow_link_in_latest_mail_for(email, email_title, start_date, options = {})
    skip_login = options[:skip_login] || false
    link = ""
    if not skip_login then
      @loginhelper.login_as(@ctx.props.admin_email, @ctx.props.admin_password)
    end
    begin
      @driver.navigate().to(@ctx.createKontrakcjaURL("/dave/backdoor?email_address=" + email + "&email_title=" + email_title + "&start_date=" + start_date))
      @h.wait_until { @driver.find_element :css => ".bodyContent a" }
      link = (@driver.find_elements :css => ".bodyContent a").first.attribute("href")
      puts "Link from latest email to " + email + " has link " + link
    ensure
      @loginhelper.logout
    end
    @driver.navigate().to(link)
  end

  def email_title(email_type)
    lang = ENV['SELENIUM_TEST_LANG']
    return EMAIL_TITLES[email_type][lang]
  end
end
