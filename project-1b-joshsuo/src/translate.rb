class Translator

  class Words 
    def initialize(pos, langs, engWord)
      @word = engWord
      @pos = pos 
      @lang = Words.parseLang(langs, engWord) 
    end

    def self.parseLang(langs, engWord)
      lang = Hash.new
      array = langs.scan(/(\w+):([\w\-]*)/)
      for i in array 
        lang[i[0]] = i[1]
      end
      lang["English"] = engWord
      return lang
    end

    def getPOS
      @pos
    end

    def getEngWord
      @word 
    end

    def getLang
      @lang
      # use words[].getLang
      # use words[].getLang["COUNTRY"] = country's word
    end

  end

  def initialize(words_file, grammar_file)
    # word file
    f = File.open(words_file)
    @words = Hash.new
    @regexWords = /^([a-z\-]+), ([A-Z]{3}), (([A-Z][a-z]*[1-9]*:[A-Z]?[a-z\-]*(, )?)+)$/

    while (line = f.gets) != nil 
      if @regexWords =~ line
        key = "#{$1}:#{$2}"
        @words[key] = Words.new($2, $3, $1)
      end
    end

    # grammar file
    f = File.open(grammar_file) 
    @grammar = Hash.new
    @regexGram = /^([A-Z]([a-z]+)?([1-9]+)?): ((([A-Z]{3})(\{([1-9])\})?(, )?)+)$/

    while (line = f.gets) != nil
      if @regexGram =~ line
        lang = $1
        array = $4.scan(/([A-Z]{3})(\{([1-9])\})?+/)
        sentStruct = Array.new
        for i in array 
          sentStruct.push(i[0])
          if(i[2] != nil)
            for j in 1...(i[2].to_i)
              sentStruct.push(i[0])
            end
          end
        end
        @grammar[lang] = sentStruct
      end
    end
  end

  # part 1

  def updateLexicon(inputfile)
    f = File.open(inputfile)

    while (line = f.gets) != nil 
      if @regexWords =~ line
        key = "#{$1}:#{$2}"
        newLang = Words.parseLang($3, $1)
        if @words.has_key?(key) 
          newLang.each do |k, v|
            @words[key].getLang[k] = v
          end
        else
          @words[key] = Words.new($2, $3, $1)
        end
      end
    end
  end

  def updateGrammar(inputfile)
    f = File.open(inputfile)

    while (line = f.gets) != nil 
      if @regexGram =~ line
        key = $1
        array = $4.scan(/([A-Z]{3})(\{([1-9])\})?+/)
        sentStruct = Array.new
        for i in array 
          sentStruct.push(i[0])
          if(i[2] != nil)
            for j in 1...(i[2].to_i)
              sentStruct.push(i[0])
            end
          end
        end
        @grammar[key] = sentStruct
      end
    end
  end

  # part 2

  def generateSentence(language, struct)
    if struct.class == String
      strArr = @grammar[struct]
    elsif struct.kind_of?(Array)
      strArr = struct
    else
      return nil
    end
    result = Array.new

    if strArr == nil
      return nil
    end
    for i in strArr
      for k, v in @words
        if v.getPOS == i
          if v.getLang.has_key?(language)
            word = v.getLang[language]
            break
          end
        end
      end
      if word == nil || word == ""
        return nil 
      end

      result.push(word)
    end
    return result.join(" ")
  end

  def checkGrammar(sentence, language)
    splitSent = sentence.split(/ /)
    gram = @grammar[language]

    if gram == nil
      return false
    end

  index = 0
    for i in gram
      checked = false
      for k,v in @words
        if i == v.getPOS
          word = v.getLang[language]
          if word == splitSent[index] || word == "" || word == nil
            checked = true
            break
          end
        end
      end
      if checked == false
        return false
      end
      index = index +1
    end
    return true
  end

  def changeGrammar(sentence, struct1, struct2)
    splitSent = sentence.split(/ /)
    
    if struct1.class == Array
      lang1 = struct1 
    elsif struct1 != Array
      lang1 = @grammar[struct1].dup
    end
    if struct2.class == Array
      lang2 = struct2
    elsif struct2.class != Array
      lang2 = @grammar[struct2].dup
    end
    if lang1.length != lang2.length
      return nil
    end
    if lang1 == lang2 && lang1.uniq.length == lang2.length
      return splitSent.join(" ")
    end
    if lang1.sort != lang2.sort
      return nil
    end

    result = ""
    leng = lang2.length
    #when [1,1,2] == [1,1,2]
    if lang1 == lang2 && lang1.uniq.length != leng
      for i in 0..leng-1
        for j in (i+1)..leng-1
          if lang1[i] == lang1[j]
            temp = splitSent[i]
            splitSent[i] = splitSent[j]
            splitSent[j] = temp
            result = splitSent.join(" ")
            return result
          end
        end
      end
    end

    #when all different locations and diff elements
    for i in 0..leng-1
      for j in 0..leng-1
        if lang1[i] == lang2[j]
          #change lang1 to match lang2
          temp = lang1[i]
          lang1[i] = lang1[j]
          lang1[j] = temp

          #change splitSent to match lang1
          temp = splitSent[i]
          splitSent[i] = splitSent[j]
          splitSent[j] = temp
        end
      end
    end
    result = splitSent.join(" ")
    return result
  end

  # part 3

  def changeLanguage(sentence, language1, language2)
    splitSent = sentence.split(/ /)
    #print splitSent
    result = Array.new
    leng = splitSent.length
      for i in 0...leng
        for k,v in @words
          if splitSent[i] == v.getLang[language1]
            word = v.getLang[language2]
          end
        end
        if word == nil
          return nil
        else
          result.push(word)
        end
      end

    return result.join(" ")
  end

  def translate(sentence, language1, language2)
    newSent = changeGrammar(sentence, language1, language2)
    if newSent == nil
      return nil
    end
    result = changeLanguage(newSent, language1, language2)
    return result
  end
end
