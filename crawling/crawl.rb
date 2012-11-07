require './config_private'

MAX_ATTEMPTS = 3

def get_user(uid) 
    num_attempts = 0
    user = NIL
    begin
        num_attempts += 1
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
    end

    return user
end

def get_cursor(uid,cur) 
    num_attempts = 0
    newcur = NIL
    begin
        num_attempts += 1
        newcur = Twitter.follower_ids(uid, options={cursor: cur})
    rescue Twitter::Error::TooManyRequests => error
        if num_attempts <= MAX_ATTEMPTS
            waittime = [error.rate_limit.reset_in,10].max
            p "(follower_id) rate limited: waiting "
            p waittime
            sleep waittime
            retry
        else
            raise "Too many rate limit fails"
        end
    end
    
    return newcur
end

def find_chain(len)
    root = 60011236
    current_user_id = root
    chain = []
    (0..len).each { |i|
        user = get_user(current_user_id)
        p user.name
        chain += [user]

        # bail if no followers
        if user.follower_count == 0 then
            return chain
        end

        # pick a random follower
        follower_index = (rand()*user.follower_count).to_i

        next_cursor = -1
        last_index = 0
        next_user_id = 0
        while next_cursor != 0 do
            cur = get_cursor(current_user_id, next_cursor)
            if last_index + cur.ids.length <= follower_index then
                last_index += cur.ids.length
            else
                within_cur_index = follower_index - last_index
                next_user_id = cur.ids[within_cur_index]
                break
            end 

            next_cursor = cur.next_cursor
        end

        current_user_id = next_user_id

    }
    return chain
end

def get_mentions(uid, max_num_tweets)
    tweets = Twitter.user_timeline(uid, options={count: max_num_tweets})
    mentions = []
    tweets.each { |t|
        mentions+=t.attrs[:entities][:user_mentions]
    }
    return mentions
end

mychain = find_chain(20)
