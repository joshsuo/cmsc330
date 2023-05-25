#//everything is class
#//OOP


a = "hello"
a.class #// == String

#//primatives work
3.class #// == Integer
3.methods #// shows all Integer methods to use

a.class.ancestors
#//shows


#//nil is nil class
nil.class #// ==NilClass


"hello".class.ancestors
Float.class.ancestors
Float.class #// == class

#############################
class creation

# @ indicates instance variables
# @@ indicates class variables
# $ indicates global variables
# instance variables are private by default
# last line in a method is returned, so don't really need return
# there is no method overloading

#//example
Class Square
    def initialize(size) #//constructor
        @size = size
    end

    def area
	@size * @size
    end
end

s = Square.new(5)
puts s.area

# end example

# example 2

class Square

    def initialize(x)
	    @size = x
	puts "I made a square"
    end

    def area
	    @size * @size
    end
end

s = Square.new(5)
puts s.area

#end example 2
#
# what happens when take out @ in area method?
# prints "i made a square" 
# size is out of scope
#


# example 3

class Square

    def initialize(x)
	    @size = x
        #shows error if global variables are not initialized
        @@pop += 1
	    puts "I made a square"
    end

    def area
	    @size * @size
    end

    # getter
    def pop 
        @@pop
    end

    # setter
    def pop = (x)
        @@pop = x
    end
end

a = Square.new(5)
b = Square.new(5)
c = Square.new(5)

# both 25
puts a.area
puts b.area

# both equal 3 because global cariables shared
puts c.pop
puts a.pop

# what if
b.pop = 5
puts b.pop
puts a.pop
# pop == 5

# what if
puts a.@@pop # syntax error
puts a.pop  # looks for a method, doesn't access pop variable, error
puts Square.pop # looks for a class method, error

#end example 3



# example 4

class Square
    attr_accessor :size
    def initialize(x)
	    @size = x
        #shows error if global variables are not initialized
	    puts "I made a square"
    end

    def area
	    @size * @size
    end
end

a=Square.new(5)
puts a.size # print 5
a.size = 1
puts a.size # print 1

# end example 4


# example 5

#returning multiple things work
def gen
    return 3,"hello"
end

a,b = gen
puts a
puts b

# end example 5



# example 6
# no method overloading

# only prints "string 2"
def str
    "string 1"
end

def str
    "string 2"
end

puts str

# end example 6

# example 7

# expects last method with same name
def str(x)
    puts x
    "string 1"
end

def str(x,y)
    puts x+y
    "string 2"
end

# produces error for wrong number of arguments
puts str(3)

# similar with
y = "hello"
y = 3
puts y
# only prints 3, same with functions as shown in example 7

# end example 7


