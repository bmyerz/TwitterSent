require 'set'

# possible metrics of interest for convergence
# followers_count
# friends_count
# status_count
# favourites_count
# listed_count  (might have too few nonzeros, requiring large sample)
# created_at (convert to account age)
# url = nil/not nil
# geo_enabled
# status=>created_at (convert to last status age)

def avg(ll)
    sum = ll.reduce(0){ |sofar,i| sofar+i }
    n = ll.length
    return sum.to_f/n
end

def var(ll)
    n = ll.length
    sum_x_sq = ll.reduce(0){ |sofar,i| sofar+i**2 }.to_f
    sum_x = ll.reduce(0){ |sofar,i| sofar+i }.to_f
    return (sum_x_sq - ((sum_x)**2)/n)/(n-1)
end

def get_attribute(sample, sym)
    ll = []
    sample.each { |user|
        ll.push(user[sym])
    }
    return ll
end


class Sample
    def initialize(fn='twitter.mhrw.all.users', start=0, num=1000000000)
        @sample = []
        i = 0
        File.open(fn, 'r') { |f|
            f.each { |line|
                i+=1
        #        if i-1 < start then next end
                puts i
                @sample.push( eval(line) )
        #        if i >= num then break end
            }
        }
    end
    
    def sample()
        return @sample
    end

    def geweke(metric, iters, xa_range = 0.10, xb_range = 0.50)
        metric_list = get_attribute(@sample[0..iters-1], metric)
        xa_end = (xa_range*metric_list.length).to_i
        xb_begin = ((1-xb_range)*metric_list.length).to_i
        xa = metric_list[0..xa_end]
        xb = metric_list[xb_begin..metric_list.length-1]

        z = (avg(xa) - avg(xb))/(Math.sqrt(var(xa) + var(xb)))  
        return z
    end

    def geweke_table(metric, stepsize, filename)
        File.open(filename, 'w') { |file|
            (0..@sample.length).step(stepsize) { |iters|
                file.puts "#{iters} #{geweke(metric,iters)}"
            }
        }
    end

    def unique_users(burn)
        s = Set.new
        get_attribute(@sample[burn..@sample.length-1], :id).each { |id|
            s.add(id)
        }

        puts "after-burn chain length: #{@sample.length-burn}"
        puts "unique samples: #{s.size}"
    end

    def avgs_and_vars()
        s = Set.new
        unique = []
        unique = @sample.select{|u| x = (not s.member?(u[:id])); s.add(u[:id]); x }

        puts "#{unique.length}"

        [:statuses_count,:friends_count,:followers_count,:listed_count].each do |metric|
            ll = get_attribute(unique, metric)
            m = avg(ll)
            sd = Math.sqrt(var(ll))
            puts "#{metric} #{m} #{sd}"
        end
    end
end
