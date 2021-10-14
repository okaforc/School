import java.util.Arrays;

public class Connect4Grid2DArray implements Connect4Grid {
    final private static char EMPTY = ' ';
    public static char[][] gameBoard = { { '|', EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, '|' },
            { '|', EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, '|' },
            { '|', EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, '|' },
            { '|', EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, '|' },
            { '|', EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, '|' },
            { '|', EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, '|' },
            { '|', '_', '_', '_', '_', '_', '_', '_', '|' }

    };

    int[] userColumns = { 1, 2, 3, 4, 5, 6, 7 }; // the column the user wants to access

    @Override
    public void emptyGrid() {
        for (int i = 0; i < gameBoard.length - 1; i++) {
            char[] cs = gameBoard[i];
            for (int j = 0; j < cs.length; j++) {
                if (Character.compare(cs[j], '|') != 0 && Character.compare(cs[j], '_') != 0) {
                    cs[j] = EMPTY;
                }
            }
        }
    }

    @Override
    public String toString() {
        String board = "";
        for (char[] cs : gameBoard) {
            board += Arrays.toString(cs).replace("[", " ").replace("]", " ").replace(",", " ");
            board += "\n";
        }
        return board;
    }

    @Override
    public boolean isValidColumn(int column) {
        for (int col : userColumns) {
            if (col == column) {
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean isColumnFull(int column) {
        for (char[] cs : gameBoard) {
            if (cs[column] == EMPTY) {
                return false;
            }
        }
        return true;
    }

    @Override
    public void dropPiece(ConnectPlayer player, int column) {
        if (isValidColumn(column) && !isColumnFull(column)) {
            for (int i = gameBoard.length - 2; i >= 0; i--) {
                char[] cs = gameBoard[i];
                if (cs[column] == EMPTY) {
                    cs[column] = (char) player.getID();d
                    break;
                }
            }
        }
    }

    @Override
    public boolean didLastPieceConnect4() {
        // check for vertical groups
        for (int i = gameBoard.length - 4; i >= 0; i--) {
            for (int j = 0; j < gameBoard[i].length; j++) {
                char c = gameBoard[i][j];
                if (Character.compare(c, EMPTY) != 0 && Character.compare(c, '|') != 0
                        && Character.compare(c, '_') != 0) {
                    if (Character.compare(c, gameBoard[i][j]) == 0 
                        && Character.compare(c, gameBoard[i + 1][j]) == 0
                            && Character.compare(c, gameBoard[i + 2][j]) == 0
                            && Character.compare(c, gameBoard[i + 3][j]) == 0) {
                        return true;
                    }
                }
            }
        }

        // check for horizontal groups
        for (int i = gameBoard.length - 2; i >= 0; i--) {
            for (int j = 0; j < gameBoard[i].length - 3; j++) {
                char c = gameBoard[i][j];
                if (Character.compare(c, EMPTY) != 0 && Character.compare(c, '|') != 0
                        && Character.compare(c, '_') != 0) {
                    if (Character.compare(c, gameBoard[i][j]) == 0 && Character.compare(c, gameBoard[i][j + 1]) == 0
                            && Character.compare(c, gameBoard[i][j + 2]) == 0
                            && Character.compare(c, gameBoard[i][j + 3]) == 0) {
                        return true;
                    }
                }
            }
        }

        // check for / diagonal groups
        for (int i = gameBoard.length - 4; i >= 0; i--) {
            for (int j = 0; j < gameBoard[i].length - 3; j++) {
                char c = gameBoard[i][j];
                if (Character.compare(c, EMPTY) != 0 && Character.compare(c, '|') != 0
                        && Character.compare(c, '_') != 0) {
                    try {
                        if (Character.compare(c, gameBoard[i][j]) == 0
                                && Character.compare(c, gameBoard[i + 1][j - 1]) == 0
                                && Character.compare(c, gameBoard[i + 2][j - 2]) == 0
                                && Character.compare(c, gameBoard[i + 3][j - 3]) == 0) {
                            return true;
                        }
                    } catch (Exception e) {
                        // e.printStackTrace();
                    }
                }
            }
        }

        // check for \ diagonal groups
        for (int i = gameBoard.length - 4; i >= 0; i--) {
            for (int j = 0; j < gameBoard[i].length - 3; j++) {
                char c = gameBoard[i][j];
                if (Character.compare(c, EMPTY) != 0 && Character.compare(c, '|') != 0
                        && Character.compare(c, '_') != 0) {
                    try {
                        if (Character.compare(c, gameBoard[i][j]) == 0
                                && Character.compare(c, gameBoard[i + 1][j + 1]) == 0
                                && Character.compare(c, gameBoard[i + 2][j + 2]) == 0
                                && Character.compare(c, gameBoard[i + 3][j + 3]) == 0) {
                            return true;
                        }
                    } catch (Exception e) {
                        // At this point, the search can't go any further, so ignore it and give up.
                    }
                }
            }
        }

        return false;
    }

    @Override
    public boolean isGridFull() {
        for (int i : userColumns) {
            if (!isColumnFull(i)) {
                return false;
            }
        }
        return true;
    }

}
