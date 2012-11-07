require 'twitter'

# This is an example config file. Create a file named config_private.rb
# in this directory, with your keys filled in. Do not share or commit your secrets.

Twitter.configure do |config|
    config.consumer_key       = 'ABC123'
    config.consumer_secret    = 'XYZ456'
    config.oauth_token        = '12345678-ABC123'
    config.oauth_token_secret = 'ABCDEFG678'
end
