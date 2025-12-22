require_relative './section-8-provided'

class Character
  def initialize hp
    @hp = hp
  end

  def resolve_encounter enc
    if !is_dead?
      play_out_encounter enc
    end
  end

  def is_dead?
    @hp <= 0
  end

  private

  def play_out_encounter enc
    self
  end
end

class Knight < Character
  attr_reader :hp, :ap

  def initialize(hp, ap)
    super hp
    @ap = ap
  end

  def to_s
    "HP: " + @hp.to_s + " AP: " + @ap.to_s
  end

  def play_out_encounter enc
    enc.knight_play_out self
  end

  def damage dam
    if @ap == 0
      Knight.new(@hp - dam, 0)
    else
      dam > @ap ? Knight.new(@hp, 0).damage((dam - @ap)) : Knight.new(@hp, @ap - dam)
    end
  end
end

class Wizard < Character
  attr_reader :hp, :mp

  def initialize(hp, mp)
    super hp
    @mp = mp
  end

  def to_s
    "HP: " + @hp.to_s + " MP: " + @mp.to_s
  end

  def play_out_encounter enc
    enc.wizard_play_out self
  end
end

class FloorTrap < Encounter
  attr_reader :dam

  def initialize dam
    @dam = dam
  end

  def to_s
    "A deadly floor trap dealing " + @dam.to_s + " point(s) of damage lies ahead!"
  end

  def knight_play_out knight
    knight.damage @dam
  end

  def wizard_play_out wizard
    wizard.mp > 0 ? Wizard.new(wizard.hp, wizard.mp - 1) : Wizard.new(wizard.hp - @dam, wizard.mp)
  end
end

class Monster < Encounter
  attr_reader :dam, :hp

  def initialize(dam, hp)
    @dam = dam
    @hp = hp
  end

  def to_s
    "A horrible monster lurks in the shadows ahead. It can attack for " +
        @dam.to_s + " point(s) of damage and has " +
        @hp.to_s + " hitpoint(s)."
  end

  def knight_play_out knight
    knight.damage @dam
  end

  def wizard_play_out wizard
    Wizard.new(wizard.hp, wizard.mp - @hp)
  end
end

class Potion < Encounter
  attr_reader :hp, :mp

  def initialize(hp, mp)
    @hp = hp
    @mp = mp
  end

  def to_s
    "There is a potion here that can restore " + @hp.to_s +
        " hitpoint(s) and " + @mp.to_s + " mana point(s)."
  end

  def knight_play_out knight
    Knight.new(@hp + knight.hp, knight.ap)
  end

  def wizard_play_out wizard
    Wizard.new(wizard.hp + @hp, wizard.mp + @mp)
  end
end

class Armor < Encounter
  attr_reader :ap

  def initialize ap
    @ap = ap
  end

  def to_s
    "A shiny piece of armor, rated for " + @ap.to_s +
        " AP, is gathering dust in an alcove!"
  end

  def knight_play_out knight
    Knight.new(knight.hp, @ap + knight.ap)
  end

  def wizard_play_out wizard
    wizard
  end
end

if __FILE__ == $0
  Adventure.new(Stdout.new, Knight.new(15, 3),
    [Monster.new(1, 1),
    FloorTrap.new(3),
    Monster.new(5, 3),
    Potion.new(5, 5),
    Monster.new(1, 15),
    Armor.new(10),
    FloorTrap.new(5),
    Monster.new(10, 10)]).play_out
end
