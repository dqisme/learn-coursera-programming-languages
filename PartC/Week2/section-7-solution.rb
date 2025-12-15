## Solution template for Guess The Word practice problem (section 7)

require_relative './section-7-provided'

class ExtendedGuessTheWordGame < GuessTheWordGame
  ## YOUR CODE HERE
end

class ExtendedSecretWord < SecretWord
  VALID_GUESS_PATTERN = /^\w$/
  GUESSES = []

  def self.match? (a_string, pattern_in_string)
    /#{pattern_in_string}/i =~ a_string
  end

  def initialize word
    self.word = word
    self.pattern = self.word.chars.map {|character| VALID_GUESS_PATTERN =~ character ? '-' : character }.join
  end

  def valid_guess? guess
    is_valid = !!(VALID_GUESS_PATTERN =~ guess) && !(ExtendedSecretWord.match? GUESSES.join, guess)
    GUESSES.push guess if is_valid
    is_valid
  end

  def guess_letter! letter
    found = ExtendedSecretWord.match? self.word, letter
    if found
      self.word.chars.each_index {|index| self.pattern[index] = self.word[index] if (ExtendedSecretWord.match? self.word[index], letter)}
    end
    found
  end
end

## Change to `false` to run the original game
if true
  ExtendedGuessTheWordGame.new(ExtendedSecretWord).play
else
  GuessTheWordGame.new(SecretWord).play
end
