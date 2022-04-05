public abstract class ConnectPlayer {
    char id;
    String moves = "";
    Connect4Grid2DArray grid = new Connect4Grid2DArray();

    public abstract int chooseCol();

    public void setID(char id) {
        this.id = id;
    }

    public int getID() {
        return id;
    }

    public void dropPiece(int col) {
        grid.dropPiece(this, col);
    }


   
}
