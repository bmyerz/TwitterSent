require 'time'

def time_str()
    return  Time.now.to_s.split.join('_')
end

def dump_cache(c, filename)
    serialized = {}
    c.each { |k,v|
        if (v.is_a?(Hash)) then
            serialized[k] = v
        else
            serialized[k] = v.attrs
        end
    }

    File.open(filename, 'w') { |f|
        f.puts "#{serialized}"
    }
end

def dumpCaches()
    dump_cache($follower_cache, "follower_cache.#{time_str()}")
    dump_cache($friend_cache, "friend_cache.#{time_str()}")
    #dump_cache($u_cache, "u_cache.#{time_str()}")
end

def loadCaches()
    File.open("follower_cache") { |f|
        attrs = eval(f.read())
        $follower_cache = attrs
    }

    File.open("friend_cache") { |f|
        attrs = eval(f.read())
        $friend_cache = attrs
    }

#    File.open("u_cache") { |f|
#        attrs = eval(f.read())
#        attrs.each { |k,v|
#            $u_cache[k] = Twitter::User.new(v)
#        }
#    }
end

    
