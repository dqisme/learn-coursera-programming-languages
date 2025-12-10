# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  All_My_Pieces = All_Pieces + [
    rotations([[0, 0], [1, 0], [0, -1], [-1, 0], [-1, -1]]),
    [
      [[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]],
      [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]
    ],
    rotations([[0, 0], [0, -1], [1, 0]])]
  # your enhancements here
  def Piece.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new([[0, 0]], board)
  end
end

class MyBoard < Board
  # your enhancements here

  def rotate_180_degrees
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def next_piece
    @current_block = @is_cheating ? MyPiece.cheat_piece(self) : Piece.next_piece(self)
    @current_pos = nil
    @is_cheating = nil
  end

  def cheat
    if !game_over? and @game.is_running? and !@is_cheating and @score >= 100
      @is_cheating = true
      @score -= 100
      @game.update_score
    end
  end
end

class MyTetris < Tetris
  # your enhancements here

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  def key_bindings
    super()
    @root.bind('u', proc {@board.rotate_180_degrees})
    @root.bind('c', proc {@board.cheat})
  end
end


