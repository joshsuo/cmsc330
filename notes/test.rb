

hm = { "one" => 2, "two" => 2, "three" => 2 }
target = 2

def num_occ(hm,target)
    hm.values.count(target)
  end

puts num_occ(hm, target)

