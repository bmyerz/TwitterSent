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

$fid_cache = {}
def get_cursor(uid,cur) 
    result = $fid_cache[[uid,cur]]
    if result then
        return result
    end

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

    $fid_cache[[uid,cur]] = newcur
    
    return newcur
end

def get_random_follower(user, prng)
    # pick a random follower
    follower_index = prng.rand(user.follower_count)

    next_cursor = -1
    last_index = 0
    next_user_id = 0
    while next_cursor != 0 do
        cur = get_cursor(user.id, next_cursor)
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



def random_walk(len)
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
            cur = get_cursor(current_user_id, next_cursor)

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
                cur = get_cursor(uid, next_cursor)

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
def mhrw(prng = Random.new(0), root, count)
    sample = []
    current_user_id = root
    num_samples = 0
    while num_samples < count do
        v = get_user(current_user_id) 

        # bail if no followers
        if v.follower_count == 0 then
            # dead end
            return [true,sample]
        end

        # pick a follower uniformly at random
        neighbor_id = get_random_follower(v, prng)
        w = get_user(neighbor_id)
        
        # reject w with probability min(1, |neigh v|/|neigh w|)
        p = prng.rand()  
        if p <= v.follower_count.to_f/w.follower_count.to_f
            $num_mhrw_accepted_samples += 1
            sample.push(w)
            num_samples += 1
            current_user_id = w.id
        else
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
       puts "collected #{sofar} of #{goal}; acc=#{$num_mhrw_accepted_samples} rej=#{$num_mhrw_rejected_samples}"

       if deadend then 
           if newsamples.empty? then
               puts "#{next_uid} was a dead end with no followers"
           else
               puts "#{newsamples[-1]} was a dead end with no followers"
           end
           break
       end
    end
end

