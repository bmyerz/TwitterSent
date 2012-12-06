require "sequel"


def prepare_user_table( tableid, db )
  
  db.create_table?(tableid) { 
      primary_key :id  #unique user id
      String :id_str  # user id as string
      Integer :followers_count
      Integer :friends_count
      String :lang
      String :time_zone
      String :screen_name
      Integer :statuses_count
      String :description
      Integer :created_at # account creation time
      Bool :verified
      String :location
      String :name
      Integer :utc_offset
  }  

  return db[tableid]
end

def insert_user( table, user )
    record = {}
    record[ :id ] = user[:id]
    record[ :id_str ] = user[:id_str]
    record[ :followers_count] = user[:followers_count]
    record[ :friends_count] = user[:friends_count]
    record[ :lang] = user[:lang]
    record[ :time_zone] = user[:time_zone]
    record[ :screen_name] = user[:screen_name]
    record[ :statuses_count] = user[:statuses_count]
    record[ :description] = user[:description]
    record[ :created_at ] = user[:created_at]
    record[ :verified] = user[:verified]
    record[ :location] = user[:location]
    record[ :name] = user[:name]
    record[ :utc_offset] = user[:utc_offset]

    begin
        table.insert(record)
    rescue Sequel::DatabaseError => e
        if e.to_s.match(/PRIMARY KEY must be unique/) then
            #puts "#{user[:id]} already in database"
            nil
        else 
            raise e
        end
    end
end

def prepare_chain_table( tableid, db )
  
  db.create_table?(tableid) { 
      primary_key :entry_id
      Integer :chain
      Integer :position
      Integer :user_id
      String :time_collected
      Integer :status_id
  }

  return db[tableid]
end

$hash_offset = 2000000
def insert_chain_entry( table, sample, chain_id, position )
    record = {}

    # primary key is chain_id,position to ensure uniqueness
    record[ :entry_id ] = chain_id*$hash_offset + position

    record[ :chain ] = chain_id
    record[ :position ] = position

    record[ :user_id ] = sample[:id]
    if sample[:status] then
        record[ :status_id ] = sample[:status][:id]
    end
    record[ :time_collected ] = sample[:time_collected]

    table.insert(record)
end
    

def prepare_tweet_table( tableid, db )
  
  db.create_table?(tableid) { 
      primary_key :id  #unique Tweet id
      String :id_str
      String :text
      String :created_at # tweet creation time (date datatype?)
      Bool :retweeted
      Integer :retweet_count
      String :source # what produced it, e.g. application, web interface, ...
      Integer :user_id    # user who posted, joins with user[:id]
      String :user_id_str # string id, joins with user[:id_str]
      String :user_mentions
      String :hashtags
      Integer :hashtags_count
      Integer :user_mentions_count

      # geo: coordinates and type 
      # place: coordinates and type
      # coordinates: coordinates and type
  }  

  return db[tableid]
end

# takes a db table, and a tweet, which is the attrs map from a Twitter::Tweet
def insert_tweet( table, tweet )
    record = {}
    # direct fields
    record[:id] = tweet[:id]
    record[:id_str] = tweet[:id_str]
    record[:text] = tweet[:text]
    record[:created_at] = tweet[:created_at]
    record[:retweeted] = tweet[:retweeted]
    record[:retweet_count] = tweet[:retweet_count]
    record[:source] = tweet[:source]

    # user fields
    user = tweet[:user]
    record[:user_id] = tweet[:user][:id]
    record[:user_id_str] = tweet[:user][:id_str]

    # other fields
    record[:hashtags] = tweet[:entities][:hashtags].collect{ |hm| hm[:text] }.join(',') # Apple and #Android -> "Apple,Android"
    record[:hashtags_count] = tweet[:entities][:hashtags].length
    record[:user_mentions] = tweet[:entities][:user_mentions].collect{ |mm| mm[:screen_name] }.join(',')
    record[:user_mentions_count] = tweet[:entities][:user_mentions].length

    table.insert(record)
end

def add_chain(dbfile, user_table_name, chain_table_name, chain, chain_id)
    db = Sequel.sqlite(dbfile)
    user_table = prepare_user_table( user_table_name, db )
    chain_table = prepare_chain_table( chain_table_name, db )

    if chain.length > $hash_offset then
        raise "Hash length assumption violated: #{chain.length}"
    end

    position = 1
    
    # write as a single transaction to speed it up
    db.transaction do # BEGIN
        chain.each { |s|
            if position%1024==0 then puts "chain #{chain_id} position #{position}" end
            insert_user( user_table, s )
            insert_chain_entry( chain_table, s, chain_id, position)
            position += 1
        }
    end # COMMIT
end

def add_chains(chain_ids, path_prefix="chain", fname="twitter.mhrw.all.compress.users")
    chain_ids.each { |i|
        puts "inserting chain #{i}"
        sample = Sample.new("#{path_prefix}#{i}/#{fname}").sample
        add_chain("twitter2.db", :users, :chains, sample, i)
    }
end



def add_all_tweets(dbfile='twitter.db', tablename, tweets)
    db = Sequel.sqlite(dbfile)
    tweets_table = prepare_tweet_table( tablename, db )
    i = 0
    tweets.each { |t|
        p i
        insert_tweet( tweets_table, t )
        i+=1
    }
end

def add_all_users_from_tweet(dbfile='twitter.db',tablename,tweets)
    db = Sequel.sqlite(dbfile)
    users_table = prepare_user_table( tablename, db )
   i = 0
    tweets.each { |t|
        p i
        insert_user( users_table, t[:user] )
        i+=1
    }
end


def add_tweets_from_log(dbfile='twitter.db', tablename=:tweets)
    tweets = []
    File.open('tweet.log', 'r') { |f|
        f.each { |line|
            tweet = eval(line)
            tweets.push(tweet)
        }
    }
    add_all_tweets(dbfile, tablename, tweets)
end

def select_ids(dbfile='twitter.db', selectfile='selected.txt', src, dest)
    db = Sequel.sqlite(dbfile)
    src_table = prepare_tweet_table( src, db )
    dest_table = prepare_tweet_table( dest, db )
    File.open(selectfile, 'r') { |sf|
        ids = sf.read.split.map{|x| x.to_i}.sort
        puts "select size #{ids.length}"
        index = 0
        src_table.each { |row|
            if index==ids.first then
                dest_table.insert( row )
                ids.delete_at(0)
            end
            index+=1
        }
    }
end


