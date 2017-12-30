class LineMap() {

    private var dummy = new Node(0, -1)
    private var root = new Node(1, dummy, dummy, Math.random().toFloat)

    //global so getColumn can see result of getRow etc
    private var p = root

    private var curline = 0

    private def length = root.treeChars

    def numLines = root.treeSize

    def getLineLength(n: Int) = {
        findLine(n)
        p.linelen
    }

    def getRow(pos: Int) = {
        findPos(pos)
        curline
    }

    def getColumn(pos: Int) = {
        val start = linestart(getRow(pos))
        pos - start
    }

    def clear() {
        val prio = Math.random().toFloat
        root = new Node(1, dummy, dummy, prio)
    }

    private def linestart(r: Int) = {
        require(0 <= r && r < numLines)
        var k = r
        p = root
        var start = p.left.treeChars
        while (k != p.left.treeSize){
            if (k < p.left.treeSize) {
                p = p.left
                start -= (p.linelen+p.right.treeChars)
            }
            else {
                k -= (p.left.treeSize+1)
                start += (p.linelen+p.right.left.treeChars)
                p = p.right
            }
        }
        start
    }

    private def split(t: Node, pos: Int):(Node, Node) = {
        if (pos == 0) {
            (dummy, t)
        }
        else if (pos == t.treeSize) {
            (t, dummy)
        }
        else if (pos <= t.left.treeSize) {
            val (q,r) = split(t.left, pos)
            t.left = r
            (q,t)
        }
        else {
            val (q,r) = split(t.right, pos-t.left.treeSize-1)
            t.right = q
            (t,r)
        }
    }

    private def insertNode(n: Int, len: Int) {
        val prio = Math.random().toFloat
        root = insertNode(root, n, len, prio)
    }

    private def insertNode(t: Node, n: Int, len: Int, prio: Float): Node = {
        if (prio > t.prio) {
            val (q, r) = split(t, n)
            new Node(len, q, r, prio)
        }
        else {
            if (n <= t.left.treeSize) {
                t.left = insertNode(t.left, n, len, prio)
            }
            else {
                t.right = insertNode(t.right, n - t.left.treeSize - 1, len, prio)
            }
            t
        }
    }

    def insert(pos: Int, ch: Char) {
        val c = getColumn(pos)
        val old = p.linelen
        if (ch != '\n') {
            deleteNode(curline)
            insertNode(curline, old+1)
        }
        else {
            deleteNode(curline)
            insertNode(curline, c+1)
            insertNode(curline+1, old-c)
        }
    }

    def insert(pos: Int, s: String) {
        val before = getColumn(pos)
        val after = p.linelen - before
        var lengths = new Array[Int](s.length+1)
        var i = 0
        for (ch <- s) {
            lengths(i) += 1
            if (ch == '\n') {
                i += 1
            }
        }
        lengths(0) += before
        lengths(i) += after
        deleteNode(curline)
        var j = 0
        while (j <= i) {
            insertNode(curline+j, lengths(j))
            j += 1
        }
    }

    def insertRange(pos: Int, t: Text, start: Int, nchars: Int) {
        val s: String = t.subSequence(start, start+nchars).toString
        insert(pos, s)
    }

    private def deleteNode(pos:Int) {
        root = deleteNode(root, pos)
    }

    private def deleteNode(t: Node, pos: Int): Node ={
        if (pos == t.left.treeSize) {
            merge(t.left, t.right)
        }
        else {
            if (pos < t.left.treeSize) {
                t.left = deleteNode(t.left, pos)
            }
            else {
                t.right = deleteNode(t.right, pos-t.left.treeSize-1)
            }
            t
        }
    }

    private def merge(t: Node, q: Node):Node = {
        if (t.prio > q.prio) {
            if (q.treeSize > 0) {
                t.right = merge(t.right, q)
            }
            t            
        }
        else {
            if (t.treeSize > 0) {
                q.left = merge(t, q.left)
            }
            q
        }
    }

    def deleteChar(pos: Int) {
        val col = getColumn(pos+1)
        val fstlen = p.linelen
        deleteNode(curline)
        if (col != 0) {
            insertNode(curline, fstlen-1)
        }
        else {
            val sndlen = getLineLength(curline)
            deleteNode(curline)
            insertNode(curline, fstlen+sndlen-1)
        }
    }

    def deleteRange(start: Int, len: Int) {
        findPos(start)
        while (start+len >= (linestart(curline)+ getLineLength(curline))) {
            var fstlen = p.linelen
            var sndlen = getLineLength(curline+1)
            deleteNode(curline)
            deleteNode(curline)
            insertNode(curline, fstlen+sndlen)
        }
        var fstlen = getLineLength(curline)
        deleteNode(curline)
        insertNode(curline, fstlen-len)
    }

    def getPos(row: Int, col: Int) = {
        val r = Math.min(Math.max(row, 0), numLines-1)
        val start:Int = linestart(r)
        val c = Math.min(Math.max(col, 0), p.linelen-1)
        start + c
    }

    def fetchLinePos(n:Int):(Int,Int) = {
        val start = linestart(n)
        return (start, p.linelen)
    }

    private def findLine(r: Int) {
        require(0 <= r && r < numLines)
        var k = r
        p = root
        while (k != p.left.treeSize){
            if (k < p.left.treeSize+1) {
                p = p.left
            }
            else {
                k -= p.left.treeSize+1
                p = p.right
            }
        }
    }

    private def findPos(pos: Int) {
        require(0 <= pos && pos < length)
        var k = pos
        p = root
        curline = p.left.treeSize
        while (k < p.left.treeChars || k >= (p.treeChars - p.right.treeChars)) {
            if (k < p.left.treeChars) {
                p = p.left
                curline -= p.right.treeSize+1
            }
            else {
                k -= (p.treeChars-p.right.treeChars)
                p = p.right
                curline += (p.treeSize - p.right.treeSize)
            }
        }
    }

    private class Node(val linelen: Int, val prio: Float) {
        private var lft: Node = null
        private var rt: Node = null
        var treeSize: Int = 0
        var treeChars: Int = 0

        def this(len: Int, lft: Node, rt: Node, prio: Float){
            this(len, prio)
            this.lft = lft
            this.rt = rt
            setTreeSize()
            setTreeChars()
        }

        private def setTreeSize() {
            treeSize = left.treeSize + right.treeSize + 1
        }

        private def setTreeChars() {
            treeChars = left.treeChars + right.treeChars + linelen
        }

        def left = lft

        def left_= (lft: Node) {
            this.lft = lft
            setTreeSize()
            setTreeChars()
        }

        def right = rt

        def right_= (rt: Node) {
            this.rt = rt
            setTreeSize()
            setTreeChars()
        }
    }

}