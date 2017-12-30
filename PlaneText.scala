// PlaneText.scala
// Copyright (c) 2015 J. M. Spivey

/** An extension of Text that keeps track of the division of the 
 * text into lines. */
class PlaneText extends Text() {

    private var linemap = new LineMap()
    
    /** Return the number of lines, including the fictitious last line. */
    def numLines = linemap.numLines
    
    /** Return the length of a line in the file */
    def getLineLength(n: Int) = linemap.getLineLength(n)
    
    /** Find the line number corresponding to a character index. */
    def getRow(pos: Int) = linemap.getRow(pos)
    
    /** Find the column number of a character index in its line. */
    def getColumn(pos: Int) = linemap.getColumn(pos)
    
    // Augment the mutator methods of Text to maintain the line map
    
    override def clear() {
        super.clear()
        linemap.clear()
    }
    
    override def insert(pos: Int, ch: Char) {
        super.insert(pos, ch)
        linemap.insert(pos, ch)
    }

    override def insert(pos: Int, s: String) {
        super.insert(pos, s)
        linemap.insert(pos, s)
    }
    
    override def insertRange(pos: Int, t: Text, start: Int, nchars: Int) {
        super.insertRange(pos, t, start, nchars)
        linemap.insertRange(pos, t, start, nchars)
    }

    override def insertFile(pos: Int, in: java.io.Reader) {
        val prevlen = length
        try { 
            super.insertFile(pos, in)
        } finally {
            val s = subSequence(pos, (length-prevlen)).toString
            linemap.insert(pos,s)
        }
    }

    override def deleteChar(pos: Int) {
        super.deleteChar(pos)
        linemap.deleteChar(pos)
    }
     
    override def deleteRange(start: Int, len: Int) {
        super.deleteRange(start, len)
        linemap.deleteRange(start, len)
    }

    /** Return the editing position closest to the specified coordinates */
    def getPos(row: Int, col: Int) = linemap.getPos(row, col)  

    /** Fetch the text of line n, without the trailing newline */
    def fetchLine(n: Int, buf: Text) = {
        var (beforestart, linelength) = linemap.fetchLinePos(n)
        getRange(beforestart, linelength-1, buf) //use beforestart not beforestart+1 as the first character in buffer is at index 0
    }    

}
