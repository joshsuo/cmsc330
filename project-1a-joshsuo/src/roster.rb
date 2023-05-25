
class Person

    def initialize(name, age)
        @name = name
        @age = age
        self
    end

    def getName
        return @name
    end

    def getAge
        return @age
    end

    def setAge(x)
        @age = x
        self
    end

end

class Student < Person
    
    def initialize(name, age, grade)
        super(name, age)
        @grade = grade 
        self
    end

    def getGrade
        return @grade 
    end

    def changeGrade(x)
        @grade = x
        self
    end

end

class Staff < Person 

    def initialize(name, age, position)
        super(name, age)
        @position = position
        self 
    end

    def getPosition
        return @position 
    end

    def changePosition(newPosition)
        @position = newPosition
        self
    end

end

class Roster

    def initialize()
        @people = Array.new
        #@size = 0
    end

    def add(person)
        @people.push(person)
        #@size += 1
    end

    def size
        return @people.length
    end

    def remove(person)
        @people.delete(person)
    end

    def getPerson(name)
        return @people.find {|person| person.getName == name}
    end

    def map
        if block_given?
            for i in @people
                yield i
            end
        end
    end

end
