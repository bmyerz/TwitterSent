require './config_private'


# Twitter uses multiJSON to allow configured json parser
# yajl is fast json parser
require 'yajl'  

MAX_ATTEMPTS = 3

$u_cache = {}
def get_user(uid) 
    result = $u_cache[uid]
    if result then
        return result
    end

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
    end

    $u_cache[uid] = user

    return user
end

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
    end

    return users
end

$follower_cache = {}
def get_followers_cursor(uid,cur) 
    result = $follower_cache[[uid,cur]]
    if result then
        return result
    end

    num_attempts = 0
    newcur = NIL
    begin
        num_attempts += 1
        puts "calling followid on #{uid}"
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

    $follower_cache[[uid,cur]] = newcur
    
    return newcur
end

$friend_cache = {}
def get_friends_cursor(uid,cur) 
    result = $friend_cache[[uid,cur]]
    if result then
        return result
    end

    num_attempts = 0
    newcur = NIL
    begin
        num_attempts += 1
        puts "calling friendid on #{uid}"
        newcur = Twitter.friend_ids(uid, options={cursor: cur})
    rescue Twitter::Error::TooManyRequests => error
        if num_attempts <= MAX_ATTEMPTS
            waittime = [error.rate_limit.reset_in,10].max
            p "(friend_id) rate limited: waiting "
            p waittime
            sleep waittime
            retry
        else
            raise "Too many rate limit fails"
        end
    end

    $friend_cache[[uid,cur]] = newcur
    
    return newcur
end

def get_random_follower(user, prng)
    # pick a random follower
    follower_index = prng.rand(user.followers_count)

    next_cursor = -1
    last_index = 0
    next_user_id = 0
    while next_cursor != 0 do
        cur = get_followers_cursor(user.id, next_cursor)
        if last_index + cur.ids.length <= follower_index then
            last_index += cur.ids.length
        else
            within_cur_index = follower_index - last_index
            next_user_id = cur.ids[within_cur_index]
            break
        end 

        next_cursor = cur.next_cursor
    end

    return next_user_id
end

def get_random_friend_or_follower(user, prng)
    # pick a random friend or follower
    ff_index = prng.rand(user.friends_count + user.followers_count)

    if ff_index >= user.friends_count then
        # choose from followers
        follower_index = ff_index - user.friends_count
        next_cursor = -1
        last_index = 0
        next_user_id = 0
        while next_cursor != 0 do
            cur = get_followers_cursor(user.id, next_cursor)
            if last_index + cur.ids.length <= follower_index then
                last_index += cur.ids.length
            else
                within_cur_index = follower_index - last_index
                next_user_id = cur.ids[within_cur_index]
                break
            end 

            next_cursor = cur.next_cursor
        end
        return next_user_id
    else
        # choose from friends
        friend_index = ff_index
        next_cursor = -1
        last_index = 0
        next_user_id = 0
        while next_cursor != 0 do
            cur = get_friends_cursor(user.id, next_cursor)
            if last_index + cur.ids.length <= friend_index then
                last_index += cur.ids.length
            else
                within_cur_index = friend_index - last_index
                next_user_id = cur.ids[within_cur_index]
                break
            end 

            next_cursor = cur.next_cursor
        end
        return next_user_id
    end
end


def random_walk(len)
    root = 60011236
    current_user_id = root
    chain = []
    (0..len).each { |i|
        user = get_user(current_user_id)
        p user.name
        chain += [user]

        # bail if no followers
        if user.followers_count == 0 then
            return chain
        end

        current_user_id = get_random_follower(user, prng)
    }
    return chain
end

require 'set'
def bfs(len, worklist = [], visited = Set.new)
    sample = []
    (0..len).each { |i|
        uid = worklist[0]
        if not uid then break end
        worklist.delete_at(0)
        visited.add(uid)
        user = get_user(uid)
        p user.name
        sample += [user]

        next_cursor = -1
        last_index = 0
        next_user_id = 0
        while next_cursor != 0 do
            # get the next page of follower ids
            cur = get_followers_cursor(current_user_id, next_cursor)

            # add unvisited users to worklist
            worklist.concat( cur.ids.select { |fid| not visited.member?(fid) } ) 
       
            # next page index 
            next_cursor = cur.next_cursor
        end
    }
    return sample
end

# determined by Twitter 
$max_users = 100

# getting users is far less rate limited
def smart_bfs(len, to_get_users = [], to_get_neighbors = [], visited = Set.new)
    sample = []
    i = 0
    while i < len do
        while i < len do
            if to_get_users.empty? then
                break
            end
            # ask Twitter for a chunk of user objects from ids list
            uids = to_get_users.slice!(0..$max_users-1)
            users = get_users(uids)
            p "got #{users.length} users"
            sample.concat(users)

            # add ids to neighbors todo
            to_get_neighbors.concat( users.map { |u| u.attrs[:id] } )
            i+=users.length
        end

        while to_get_users.empty? do
            if to_get_neighbors.empty? then 
                return sample
            end
            uid = to_get_neighbors.slice!(0)

            next_cursor = -1
            last_index = 0
            next_user_id = 0
            while next_cursor != 0 do
                # get the next page of follower ids
                cur = get_followers_cursor(uid, next_cursor)

                # add never-before-seen users to worklist
                newusers = cur.ids.select { |fid| not visited.member?(fid) }
                newusers.each { |u| visited.add(u) }
                to_get_users.concat( newusers ) 

                # next page index 
                next_cursor = cur.next_cursor
            end
        end
    end
    
    return sample
end

$num_mhrw_rejected_samples = 0
$num_mhrw_accepted_samples = 0
$num_mhrw_protected_samples = 0
def mhrw(prng = Random.new(0), root, count)
    sample = []
    current_user_id = root
    num_samples = 0
    while num_samples < count do
        v = get_user(current_user_id) 

        # bail if no followers or friends (this should never happen)
        if v.followers_count+v.friends_count == 0 then
            # dead end
            return [true,sample]
        end

        # pick a neighbor (friend or follower) uniformly at random
        neighbor_id = get_random_friend_or_follower(v, prng)
        w = get_user(neighbor_id)

        # reject w if protected account
        # Twitter API will not allow calls to followers/ids or friends/ids
        # only getting the user info.
        # This practical concern changes the effective in/out degree of 
        # neighbor nodes. Hopefully proportion of protected neighbors is independent.
        if w.protected then
            $num_mhrw_protected_samples += 1
            # stay at v
            next 
        end
        
        # reject w with probability min(1, |neigh v|/|neigh w|)
        p = prng.rand()  
        kv = v.friends_count + v.followers_count
        kw = w.friends_count + w.followers_count
        if p <= kv.to_f/kw.to_f
            $num_mhrw_accepted_samples += 1
            sample.push(w)
            num_samples += 1
            current_user_id = w.id
        else
            puts "reject #{w.name} because kv=#{kv} / kw=#{kw}"
            $num_mhrw_rejected_samples += 1
            # stay at v
        end
    end

    # not dead end
    return [false,sample]
end


def get_mentions(uid, max_num_tweets)
    tweets = Twitter.user_timeline(uid, options={count: max_num_tweets})
    mentions = []
    tweets.each { |t|
        mentions+=t.attrs[:entities][:user_mentions]
    }
    return mentions
end

#mychain = random_walk(20)

def collect_smart( root = 60011236, goal = 10000 )
    bfs_todo_userobject = [root]
    bfs_todo_getneighbors = []
    bfsvisited = Set.new
    bfsvisited.add(root)
    File.open('twitter.bfs.users', 'a') { |f|
        f.puts "---- new collection ----"
    }
    increment = 100
    sofar = 0
    rounds = 0
    while sofar < goal do
        rounds += 1
        newsamples = smart_bfs(increment, bfs_todo_userobject, bfs_todo_getneighbors, bfsvisited)
        # write to disk after every #{sofar} to not lose data
        File.open('twitter.bfs.users', 'a') { |f|
            newsamples.each { |user|
                f.puts "#{user.attrs}" # write the json string
            }
        }
        sofar += newsamples.length
        puts "collected #{sofar} of #{goal} after #{rounds} rounds"
    end
end

require 'time'
$mhrw_fn = 'twitter.mhrw.users'
def collect_mhrw( root, goal = 100000 )
    prng = Random.new(Time.now.usec)
    File.open($mhrw_fn, 'a') { |f|
        f.puts "---- new collection ----"
        f.puts "---- started at uid=#{root} ----"
    }

    next_uid = root
    increment = 1
    sofar = 0
    rounds = 0
    while sofar < goal do
       rounds += 1
       deadend,newsamples = mhrw( prng, next_uid, increment )
       if not deadend then next_uid = newsamples[-1] end

       #write samples to disk
       File.open($mhrw_fn, 'a') { |f|
           newsamples.each { |user|
               # include approximate time of collection
               user.attrs[:time_collected] = Time.now.to_s
               f.puts "#{user.attrs}" # write the json string
           }
       }
       sofar += newsamples.length
       puts "collected #{sofar} of #{goal}; acc=#{$num_mhrw_accepted_samples} rej=#{$num_mhrw_rejected_samples} prot=#{$num_mhrw_protected_samples}"

       if deadend then 
           if newsamples.empty? then
               puts "#{next_uid} was a dead end with no followers"
           else
               puts "#{newsamples[-1]} was a dead end with no followers"
           end
           raise "Dead end"
       end
    end
end

