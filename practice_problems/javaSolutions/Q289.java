import java.util.*;

import myUtils.*;

public class Q289 {
  public void gameOfLife(int[][] board) {
    if (board == null || board.length < 1 || board[0].length < 1)
      return;

    lifeRecurse(board, 0);
  }

  private void lifeRecurse(int[][] board, int cell) {
    int col = cell % board[0].length;
    int row = (cell - col) / board[0].length;

    System.out.format("Cell: %d, Row: %d, Col: %d\n", cell, row, col);

    int status = board[row][col];
    int deadNei = 0;
    if (row > 0) {
      if (board[row - 1][col] == 0)
        ++deadNei;

      if (col > 0 && board[row - 1][col - 1] == 0)
        ++deadNei;

      if (col < board[0].length - 1 && board[row - 1][col + 1] == 0)
        ++deadNei;
    }

    if (row < board.length - 1) {
      if (board[row + 1][col] == 0)
        ++deadNei;

      if (col > 0 && board[row + 1][col - 1] == 0)
        ++deadNei;

      if (col < board[0].length - 1 && board[row + 1][col + 1] == 0)
        ++deadNei;
    }

    if (col > 0 && board[row][col - 1] == 0)
      ++deadNei;

    if (col < board[0].length - 1 && board[row][col + 1] == 0)
      ++deadNei;

    int neighbours = 0, liveNei = 0;
    if (board.length == 1 && board[0].length == 1)
      neighbours = 0;

    else if (board.length == 1) {
      if (col == 0 || col == board[0].length - 1)
        neighbours = 1;
      else
        neighbours = 2;

    } else if (board[0].length == 1) {
      if (row == 0 || row == board.length - 1)
        neighbours = 1;
      else
        neighbours = 2;

    } else {
      if ((row == 0 && col == 0)
          || (row == 0 && col == board[0].length - 1)
          || (col == 0 && row == board.length - 1)
          || (row == board.length - 1 && col == board[0].length - 1))
        neighbours = 3;

      else if (row == 0 || row == board.length - 1
               || col == 0 || col == board[0].length - 1)
        neighbours = 5;

      else
        neighbours = 8;

      liveNei = neighbours - deadNei;
    }

    // Recurse here
    if (cell < board.length * board[0].length - 1)
      lifeRecurse(board, cell + 1);

    if (status == 0) {
      if (liveNei == 3)
        board[row][col] = 1;
    } else {
      if (liveNei < 2 || liveNei > 3)
        board[row][col] = 0;
    }
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q289 sol = new Q289();
    //int[][] test = {{0}};
    //sol.gameOfLife(test);
    //System.out.println(Arrays.toString(test[0]));
    int[][] test2 = {{0,0,0,0},
                     {0,1,1,0},
                     {0,1,1,0},
                     {0,0,0,0}};
    sol.gameOfLife(test2);
    for (int i = 0; i < test2.length; ++i)
      System.out.println(Arrays.toString(test2[i]));
  }
}
