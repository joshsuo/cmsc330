def fib(n)

    if n == 0
        return []
    elsif n == 1
        return [0]
    elsif n == 2
        return [0, 1]
    else
        arr = Array.new(n-1)
        num1 = 0
        num2 = 1
        arr[0] = num1
        arr[1] = num2

        for i in 2..(n-1)
            num3 = num1 + num2
            arr[i] = num3

            num1 = num2
            num2 = num3
        end
        return arr
    end
end

def isPalindrome(n)

    num = n.to_s

    if num == num.reverse
        return true
    else
        return false
    end
            
end

def nthmax(n, a)

    if a.length < n
        return nil
    else
        for i in 0..a.length-2
            for j in (i+1)..a.length-1
                if a[i] < a[j]
                    temp = a[i]
                    a[i] = a[j]
                    a[j] = temp
                end
            end
        end
    end
    return a[n]
end

def freq(s)

    if s == ""
        return ""
    else
        str = s.delete(" ")
        arr = str.split("")
        hash = Hash.new

        for i in 0..(arr.length-1)
            if hash.has_key?(arr[i])
                hash[arr[i]] += 1
            else
                hash[arr[i]] = 1
            end
        end
        return hash.key(hash.values.max)
    end
end

def zipHash(arr1, arr2)

    h = Hash.new
    if arr1.length() != arr2.length()
        return nil
    else
        for i in 0..arr1.length()-1
            h[arr1[i]] = arr2[i]
        end
    end
    return h
end

def hashToArray(hash)

    arr = Array.new(hash.length()){Array.new(2)}
    
    for i in 0..(hash.length()-1)
        arr[i][0] = hash.keys[i]
        arr[i][1] = hash[arr[i][0]]
    end
    return arr
end

def maxProcChain(init, procs)
    max = init
    
    for i in 0...procs.length
        curr = init
        total = init
        for j in i...procs.length
            total = procs[j].call(total)

            if max < procs[j].call(curr)
                max = procs[j].call(curr)
                curr = procs[j].call(curr)
            end
            
            if max < total 
                max = total
                curr = total
            end
        end
    end
    return max
end

