require './config_private'
require 'set'
require 'yajl'

$error_buf = nil
$per_request = 25 #100  #lower raises chance of bulk success
$prng = Random.new(Time.now.usec)

$sample_fn = "twitter.rs.users"

MAX_ATTEMPTS = 3
FALLBACK = 0xDEADBEEF

def get_users(uidlist) 
    num_attempts = 0
    user = NIL
    begin
        num_attempts += 1
        users = Twitter.users(uidlist)
    rescue Twitter::Error::TooManyRequests => error
        if num_attempts <= MAX_ATTEMPTS
            waittime = [error.rate_limit.reset_in,10].max
            p "(users) rate limited: waiting "
            p waittime
            sleep waittime
            retry
        else
            raise "Too many rate limit fails"
        end
    rescue Twitter::Error::NotFound => error
        return FALLBACK
    rescue Twitter::Error::AlreadyRetweeted => error
        return FALLBACK
    end

    return users
end

$u_cache = {}
def get_user(uid) 
    result = $u_cache[uid]
    if result then
        # catch an anomaly where sometimes the user data is incomplete
        # note: most notable when discovered through edges, but we don't mind rejecting an inactive user as well in id sampling case
        if not (result.statuses_count==0 and result.followers_count==0 and result.friends_count==0) then
            return result
        else
            $u_cache[uid] = nil
        end
    end

    while true do

        num_attempts = 0
        user = NIL
        begin
            num_attempts += 1
            puts "calling user on #{uid}"
            user = Twitter.user(uid)
        rescue Twitter::Error::TooManyRequests => error
            if num_attempts <= MAX_ATTEMPTS
                waittime = [error.rate_limit.reset_in,10].max
                p "(user) rate limited: waiting "
                p waittime
                sleep waittime
                retry
            else
                raise "Too many rate limit fails"
            end
        rescue Twitter::Error::NotFound => error
            $u_cache[uid] = nil
            return nil
        rescue Twitter::Error::AlreadyRetweeted => error
            $u_cache[uid] = nil
            return nil
        end

        # catch an anomaly where sometimes the user data is incomplete
        if user.statuses_count==0 and user.followers_count==0 and user.friends_count==0 then
            next
        else
            $u_cache[uid] = user
            return user
        end
    end
end

$ids_last = nil
def collect_rs(twitter_size=500000000, upper_bound=1000000000)
   accepted = 0
   rejected = 0
    File.open($sample_fn, 'a') { |file|
        file.puts "---- new collection ----"
    }

   while (true) do

     ids_tried = (0..$per_request).collect(){|ignore|$prng.rand(upper_bound)}
     $ids_last = ids_tried
     
     # first try in bulk
     valid = get_users(ids_tried)     
     valid_result = valid

     if valid_result == FALLBACK then
         puts "falling back to user/show"
         valid = []
         ids_tried.each{|id|
             u = get_user(id)
             if u != nil then
                 valid.push(u)
             end
         }
     end


     ids_valid = valid.collect(){|user| user.attrs[:id]}

     # reverse map id=>user
     vd = {}
     valid.each {|user|
        vd[user.attrs[:id]] = user
     }
    
     tried_set = Set.new
     ids_tried.each {|i| tried_set.add(i)}

     valid_set = Set.new
     ids_valid.each {|i| valid_set.add(i)} 

     # sanity check to see that (valid C tried)
     if not valid_set.subset?(tried_set) then
         $error_buf = {tried: ids_tried, valid: ids_valid}
         raise "Incorrect return, see $error_buf"
     end

     # write all users for trieds that are valid;
     # makes sure to get duplicates in tried
     puts "writing..."
     File.open($sample_fn, 'a') { |file|
         ids_tried.each { |tried|
             if valid_set.member?(tried) then
                 accepted += 1
                 file.puts "#{vd[tried].attrs}"
             else
                 rejected +=1
             end
         } 
     }

     puts "estimated accept rate=#{twitter_size.to_f/upper_bound.to_f}; actual accept rate=#{accepted.to_f/(accepted+rejected)}; tries=#{accepted+rejected}"
     
   end
end
    

