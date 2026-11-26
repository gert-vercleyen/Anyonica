/*
 * SparseNormalForms.java
 *
 * Smith and Hermite normal forms of sparse integer matrices, together with the
 * transformation matrices and the inverses of the transformation matrices:
 *
 *   smith():    u . a . v == d     gives  u, u^-1, d, v, v^-1
 *   hermite():  u . a     == h     gives  u, u^-1, h
 *
 * All arithmetic is exact: 64 bit integers where they suffice, BigInteger
 * where they do not.
 *
 * This file is the source of SparseNormalForms.jar. The jar is used through
 * J/Link by SparseNormalForms.m; the three files belong in the same directory
 * (Kernel/ReductionTools). The loader of the package only reads Wolfram
 * Language files, so this file and the jar do not disturb it.
 *
 * To rebuild the jar after a change, with any JDK, in this directory:
 *
 *   mkdir snf-classes
 *   javac --release 8 -d snf-classes SparseNormalForms.java
 *   jar cf SparseNormalForms.jar -C snf-classes anyonica
 *
 * and delete the directory snf-classes. "--release 8" makes the jar usable with
 * every Java runtime from version 8 on, whichever one Mathematica brings along
 * (leave it out with a version 8 JDK; a JDK that no longer accepts 8 will say
 * which lowest value it accepts). A kernel that has already used the old jar
 * keeps using it until ReinstallJava[] is evaluated or the kernel is restarted.
 * If the methods that SparseNormalForms.m calls change, raise VERSION here and
 * $javaClassVersion there.
 *
 * The class has no dependencies. If the J/Link classes are present they are
 * used, by reflection, to notice that the Mathematica evaluation was aborted.
 */
package anyonica;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.PriorityQueue;

public final class SparseNormalForms {

    /** Version of this class. SparseNormalForms.m checks it to detect an outdated jar. */
    public static final int VERSION = 2;

    /* ====================================================================== */
    /*  Sparse vectors                                                         */
    /* ====================================================================== */

    /**
     * Values are kept as long while their absolute value is at most LIM, so that
     * the sum of two of them cannot overflow. Larger values switch the vector to
     * BigInteger; makeBig switches back when everything fits again.
     */
    static final long LIM = 1L << 61;
    static final int LIMBITS = 61;

    /** Immutable sparse vector: n entries, increasing indices, long or BigInteger values. */
    static final class Vec {
        final int n;
        final int[] idx;
        final long[] lv;
        final BigInteger[] bv;

        Vec(int n, int[] idx, long[] lv, BigInteger[] bv) {
            this.n = n;
            this.idx = idx;
            this.lv = lv;
            this.bv = bv;
        }

        /** Position of index c, or -1. */
        int find(int c) {
            int lo = 0, hi = n - 1;
            int[] a = idx;
            while (lo <= hi) {
                int mid = (lo + hi) >>> 1;
                int v = a[mid];
                if (v < c) lo = mid + 1;
                else if (v > c) hi = mid - 1;
                else return mid;
            }
            return -1;
        }

        BigInteger bval(int k) {
            return bv != null ? bv[k] : BigInteger.valueOf(lv[k]);
        }
    }

    static final Vec ZERO = new Vec(0, new int[0], new long[0], null);

    static Vec unit(int i) {
        return new Vec(1, new int[] { i }, new long[] { 1L }, null);
    }

    private static Vec makeLong(int n, int[] idx, long[] lv) {
        if (n == 0) return ZERO;
        if (n < idx.length - (idx.length >> 2)) {
            idx = Arrays.copyOf(idx, n);
            lv = Arrays.copyOf(lv, n);
        }
        return new Vec(n, idx, lv, null);
    }

    private static Vec makeBig(int n, int[] idx, BigInteger[] bv) {
        if (n == 0) return ZERO;
        boolean fits = true;
        for (int i = 0; i < n; i++) {
            if (bv[i].bitLength() > LIMBITS) {
                fits = false;
                break;
            }
        }
        if (fits) {
            long[] lv = new long[n];
            for (int i = 0; i < n; i++) lv[i] = bv[i].longValue();
            return new Vec(n, n == idx.length ? idx : Arrays.copyOf(idx, n), lv, null);
        }
        if (n < idx.length) {
            idx = Arrays.copyOf(idx, n);
            bv = Arrays.copyOf(bv, n);
        }
        return new Vec(n, idx, null, bv);
    }

    /** x + a y */
    static Vec axpy(Vec x, long a, Vec y) {
        if (a == 0 || y.n == 0) return x;
        if (x.bv == null && y.bv == null) {
            Vec r = axpyLong(x, a, y);
            if (r != null) return r;
        }
        return axpyBig(x, BigInteger.valueOf(a), y);
    }

    /** x + a y */
    static Vec axpy(Vec x, BigInteger a, Vec y) {
        if (a.bitLength() <= 62) return axpy(x, a.longValue(), y);
        if (y.n == 0) return x;
        return axpyBig(x, a, y);
    }

    /** x + a y in long arithmetic; null if a value leaves the range [-LIM, LIM]. */
    private static Vec axpyLong(Vec x, long a, Vec y) {
        int nx = x.n, ny = y.n;
        int[] xi = x.idx, yi = y.idx;
        long[] xv = x.lv, yv = y.lv;
        int[] ri = new int[nx + ny];
        long[] rv = new long[nx + ny];
        int i = 0, j = 0, k = 0;
        try {
            if (a == 1 || a == -1) {
                while (i < nx && j < ny) {
                    int ci = xi[i], cj = yi[j];
                    if (ci < cj) {
                        ri[k] = ci;
                        rv[k++] = xv[i++];
                    } else if (ci > cj) {
                        ri[k] = cj;
                        rv[k++] = a == 1 ? yv[j] : -yv[j];
                        j++;
                    } else {
                        long s = a == 1 ? xv[i] + yv[j] : xv[i] - yv[j];
                        if (s > LIM || s < -LIM) return null;
                        if (s != 0) {
                            ri[k] = ci;
                            rv[k++] = s;
                        }
                        i++;
                        j++;
                    }
                }
                while (j < ny) {
                    ri[k] = yi[j];
                    rv[k++] = a == 1 ? yv[j] : -yv[j];
                    j++;
                }
            } else {
                while (i < nx && j < ny) {
                    int ci = xi[i], cj = yi[j];
                    if (ci < cj) {
                        ri[k] = ci;
                        rv[k++] = xv[i++];
                    } else {
                        long s = Math.multiplyExact(a, yv[j]);
                        if (ci == cj) {
                            s = Math.addExact(s, xv[i]);
                            i++;
                        }
                        if (s > LIM || s < -LIM) return null;
                        if (s != 0) {
                            ri[k] = cj;
                            rv[k++] = s;
                        }
                        j++;
                    }
                }
                while (j < ny) {
                    long s = Math.multiplyExact(a, yv[j]);
                    if (s > LIM || s < -LIM) return null;
                    ri[k] = yi[j];
                    rv[k++] = s;
                    j++;
                }
            }
        } catch (ArithmeticException e) {
            return null;
        }
        while (i < nx) {
            ri[k] = xi[i];
            rv[k++] = xv[i++];
        }
        return makeLong(k, ri, rv);
    }

    private static Vec axpyBig(Vec x, BigInteger a, Vec y) {
        int nx = x.n, ny = y.n;
        int[] xi = x.idx, yi = y.idx;
        int[] ri = new int[nx + ny];
        BigInteger[] rv = new BigInteger[nx + ny];
        int i = 0, j = 0, k = 0;
        while (i < nx || j < ny) {
            int ci = i < nx ? xi[i] : Integer.MAX_VALUE;
            int cj = j < ny ? yi[j] : Integer.MAX_VALUE;
            if (ci < cj) {
                ri[k] = ci;
                rv[k++] = x.bval(i++);
            } else {
                BigInteger s = a.multiply(y.bval(j));
                if (ci == cj) {
                    s = s.add(x.bval(i));
                    i++;
                }
                if (s.signum() != 0) {
                    ri[k] = cj;
                    rv[k++] = s;
                }
                j++;
            }
        }
        return makeBig(k, ri, rv);
    }

    static Vec neg(Vec x) {
        return axpy(ZERO, -1L, x);
    }

    /** a x + b y */
    static Vec lincomb(BigInteger a, Vec x, BigInteger b, Vec y) {
        return axpy(axpy(ZERO, a, x), b, y);
    }

    /* ====================================================================== */
    /*  Small helper classes                                                   */
    /* ====================================================================== */

    /**
     * A result matrix as lists of row indices, column indices and values (indices
     * start at 1). The values are long until one of them needs more than 64 bits.
     */
    static final class Coo {
        static final long MAX = 2147483631L;
        int nnz;
        int[] r;
        int[] c;
        long[] lv;
        BigInteger[] bv;

        /** size: the number of entries if it is known, otherwise a first guess. */
        Coo(long size) {
            if (size > MAX) throw new IllegalStateException("result has more than 2^31 nonzero entries");
            r = new int[(int) size];
            c = new int[(int) size];
            lv = new long[(int) size];
        }

        private void position(int row, int col) {
            if (nnz == r.length) {
                long cap = Math.min(MAX, Math.max(16L, 2L * r.length));
                if (nnz >= cap) throw new IllegalStateException("result has more than 2^31 nonzero entries");
                r = Arrays.copyOf(r, (int) cap);
                c = Arrays.copyOf(c, (int) cap);
                if (lv != null) lv = Arrays.copyOf(lv, (int) cap);
                if (bv != null) bv = Arrays.copyOf(bv, (int) cap);
            }
            r[nnz] = row;
            c[nnz] = col;
        }

        void add(int row, int col, long x) {
            position(row, col);
            if (bv != null) bv[nnz] = BigInteger.valueOf(x);
            else lv[nnz] = x;
            nnz++;
        }

        void add(int row, int col, BigInteger x) {
            position(row, col);
            if (bv == null && x.bitLength() <= 63) {
                lv[nnz++] = x.longValue();
                return;
            }
            if (bv == null) {
                bv = new BigInteger[r.length];
                for (int i = 0; i < nnz; i++) bv[i] = BigInteger.valueOf(lv[i]);
                lv = null;
            }
            bv[nnz++] = x;
        }

        void add(int row, int col, Vec v, int k) {
            if (v.bv == null) add(row, col, v.lv[k]);
            else add(row, col, v.bv[k]);
        }
    }

    /** Queue entry of a row: smallest absolute value in the row, then the cost of using the row. */
    static final class Ent implements Comparable<Ent> {
        final long key;
        final BigInteger bkey;
        final int len;
        final int row;
        final int ver;

        Ent(long key, BigInteger bkey, int len, int row, int ver) {
            this.key = key;
            this.bkey = bkey;
            this.len = len;
            this.row = row;
            this.ver = ver;
        }

        @Override
        public int compareTo(Ent o) {
            if (key != o.key) return key < o.key ? -1 : 1;
            if (bkey != null && o.bkey != null) {
                int c = bkey.compareTo(o.bkey);
                if (c != 0) return c;
            }
            if (len != o.len) return len < o.len ? -1 : 1;
            return row < o.row ? -1 : row > o.row ? 1 : 0;
        }

        boolean sameKey(Ent o) {
            return key == o.key && (bkey == null || bkey.equals(o.bkey));
        }
    }

    static final class IntList {
        int[] a = new int[4];
        int size;

        void add(int x) {
            if (size == a.length) a = Arrays.copyOf(a, size * 2);
            a[size++] = x;
        }
    }

    /** Thrown inside smith() and hermite() to leave the calculation when it is aborted; see poll(). */
    private static final class AbortedException extends RuntimeException {
        private static final long serialVersionUID = 1L;

        AbortedException() {
            super("calculation aborted");
        }
    }

    /* ====================================================================== */
    /*  State                                                                  */
    /* ====================================================================== */

    /* the input matrix, indices starting at 0 */
    private int m;
    private int n;
    private int[] inR;
    private int[] inC;
    private long[] inL;
    private BigInteger[] inB;

    /* work data */
    private Vec[] A;      // rows of the matrix that is being reduced
    private Vec[] U;      // rows of the left transformation
    private Vec[] Wt;     // columns of the inverse of the left transformation
    private Vec[] Vt;     // columns of the right transformation (Smith only)
    private Vec[] Vi;     // rows of the inverse of the right transformation (Smith only)
    private IntList[] colRows;   // per column: rows that have, or had, an entry there
    private int[] colCnt;        // per column: number of entries in rows that are not done
    private int[] ver;           // per row: version, to recognise outdated queue entries
    private boolean[] done;
    private int[] stamp;
    private int curStamp;
    private PriorityQueue<Ent> pq;
    private boolean usePQ;
    private long rowOps;
    private long diagonalSteps;
    private boolean usedBig;
    private long qL;             // the current quotient: qL if qB == null, else qB
    private BigInteger qB;
    private long lastCost;
    private static final int CANDIDATES = 12;
    private IntList pivRow;
    private IntList pivCol;
    private BigInteger[] pivVal = new BigInteger[16];
    private int[] owner;         // Hermite: per column the row that has its pivot there, or -1

    /* a pending update  x += sum_k q_k y_k  of one vector (see combine) */
    private int batchN;
    private int[] batchIdx = new int[16];
    private long[] batchQL = new long[16];
    private BigInteger[] batchQB = new BigInteger[16];

    /* results */
    private int rank = -1;
    private Coo resU;
    private Coo resUI;
    private Coo resD;
    private Coo resV;
    private Coo resVI;

    /* abort handling */
    private static boolean jlinkLookedUp;
    private static java.lang.reflect.Method jlinkGetLink;
    private static java.lang.reflect.Method jlinkWasInterrupted;
    private int ticks;
    private boolean aborted;

    public SparseNormalForms() {
    }

    public int getVersion() {
        return VERSION;
    }

    /* ====================================================================== */
    /*  Input                                                                  */
    /* ====================================================================== */

    /**
     * Sets the matrix: m rows, n columns, entry vals[k] at row rows[k], column
     * cols[k] (both starting at 1). Repeated positions are added up.
     */
    public void setMatrix(int m, int n, int[] rows, int[] cols, int[] vals) {
        long[] lv = new long[vals.length];
        for (int i = 0; i < vals.length; i++) lv[i] = vals[i];
        store(m, n, rows, cols, lv, null);
    }

    /** As setMatrix, with the values given as decimal numbers separated by white space. */
    public void setMatrixBig(int m, int n, int[] rows, int[] cols, String vals) {
        String t = vals.trim();
        String[] parts = t.isEmpty() ? new String[0] : t.split("\\s+");
        BigInteger[] bv = new BigInteger[parts.length];
        for (int i = 0; i < parts.length; i++) bv[i] = new BigInteger(parts[i]);
        store(m, n, rows, cols, null, bv);
    }

    private void store(int m, int n, int[] rows, int[] cols, long[] lv, BigInteger[] bv) {
        int len = rows.length;
        if (cols.length != len || (lv != null ? lv.length : bv.length) != len)
            throw new IllegalArgumentException("row, column and value lists have different lengths");
        if (m < 0 || n < 0) throw new IllegalArgumentException("negative dimensions");
        int[] r = new int[len];
        int[] c = new int[len];
        for (int i = 0; i < len; i++) {
            if (rows[i] < 1 || rows[i] > m || cols[i] < 1 || cols[i] > n)
                throw new IllegalArgumentException("position {" + rows[i] + ", " + cols[i]
                        + "} lies outside a " + m + " x " + n + " matrix");
            r[i] = rows[i] - 1;
            c[i] = cols[i] - 1;
        }
        this.m = m;
        this.n = n;
        inR = r;
        inC = c;
        inL = lv;
        inB = bv;
        clearResults();
    }

    private static Vec[] buildRows(int nrows, int[] r, int[] c, long[] lv, BigInteger[] bv) {
        int len = r.length;
        int[] start = new int[nrows + 1];
        for (int i = 0; i < len; i++) start[r[i] + 1]++;
        for (int i = 0; i < nrows; i++) start[i + 1] += start[i];
        long[] keys = new long[len];
        int[] pos = Arrays.copyOf(start, nrows);
        for (int i = 0; i < len; i++) keys[pos[r[i]]++] = ((long) c[i] << 32) | (long) i;
        Vec[] rows = new Vec[nrows];
        for (int i = 0; i < nrows; i++) {
            int a = start[i], b = start[i + 1];
            if (a == b) {
                rows[i] = ZERO;
                continue;
            }
            Arrays.sort(keys, a, b);
            int[] idx = new int[b - a];
            BigInteger[] vals = new BigInteger[b - a];
            int k = 0;
            for (int t = a; t < b; t++) {
                int col = (int) (keys[t] >>> 32);
                int src = (int) keys[t];
                BigInteger v = bv != null ? bv[src] : BigInteger.valueOf(lv[src]);
                if (k > 0 && idx[k - 1] == col) {
                    vals[k - 1] = vals[k - 1].add(v);
                } else {
                    idx[k] = col;
                    vals[k++] = v;
                }
            }
            int kk = 0;
            for (int t = 0; t < k; t++) {
                if (vals[t].signum() != 0) {
                    idx[kk] = idx[t];
                    vals[kk++] = vals[t];
                }
            }
            rows[i] = makeBig(kk, idx, vals);
        }
        return rows;
    }

    /* ====================================================================== */
    /*  Work structures                                                        */
    /* ====================================================================== */

    private static Vec[] identity(int size) {
        Vec[] e = new Vec[size];
        for (int i = 0; i < size; i++) e[i] = unit(i);
        return e;
    }

    private void init(boolean right) {
        clearResults();
        A = buildRows(m, inR, inC, inL, inB);
        colRows = new IntList[n];
        colCnt = new int[n];
        ver = new int[m];
        done = new boolean[m];
        stamp = new int[m];
        curStamp = 0;
        rowOps = 0;
        diagonalSteps = 0;
        usedBig = false;
        batchN = 0;
        for (int i = 0; i < m; i++) {
            Vec r = A[i];
            for (int k = 0; k < r.n; k++) {
                int c = r.idx[k];
                if (colRows[c] == null) colRows[c] = new IntList();
                colRows[c].add(i);
                colCnt[c]++;
            }
        }
        U = identity(m);
        Wt = identity(m);
        if (right) {
            Vt = identity(n);
            Vi = identity(n);
        } else {
            Vt = null;
            Vi = null;
        }
    }

    private int nextStamp() {
        if (++curStamp == Integer.MAX_VALUE) {
            Arrays.fill(stamp, 0);
            curStamp = 1;
        }
        return curStamp;
    }

    /** Replaces row i of A and keeps the column counts, the column lists and the queue up to date. */
    private void setRow(int i, Vec v) {
        Vec old = A[i];
        A[i] = v;
        if (v.bv != null) usedBig = true;
        int a = 0, b = 0;
        int na = old.n, nb = v.n;
        int[] ia = old.idx, ib = v.idx;
        while (a < na || b < nb) {
            int ca = a < na ? ia[a] : Integer.MAX_VALUE;
            int cb = b < nb ? ib[b] : Integer.MAX_VALUE;
            if (ca == cb) {
                a++;
                b++;
            } else if (ca < cb) {
                colCnt[ca]--;
                a++;
            } else {
                colCnt[cb]++;
                addToColumn(cb, i);
                b++;
            }
        }
        ver[i]++;
        if (usePQ) push(i);
    }

    private void addToColumn(int c, int row) {
        IntList l = colRows[c];
        if (l == null) {
            l = new IntList();
            colRows[c] = l;
        }
        l.add(row);
        if (l.size > 2 * colCnt[c] + 16) column(c);
    }

    /** The rows that are not done and have an entry in column c. Cleans up the list of the column. */
    private int[] column(int c) {
        IntList l = colRows[c];
        if (l == null) return new int[0];
        int s = nextStamp();
        int k = 0;
        int[] a = l.a;
        for (int t = 0; t < l.size; t++) {
            int r = a[t];
            if (stamp[r] == s) continue;
            if (done[r]) continue;
            if (A[r].find(c) < 0) continue;
            stamp[r] = s;
            a[k++] = r;
        }
        l.size = k;
        return Arrays.copyOf(a, k);
    }

    /* ====================================================================== */
    /*  Quotients and row operations                                           */
    /* ====================================================================== */

    private boolean qIsZero() {
        return qB == null ? qL == 0 : qB.signum() == 0;
    }

    /**
     * Sets the current quotient to x[kx] / y[ky], rounded down if floor is true
     * and rounded to the nearest integer otherwise.
     */
    private void quotient(Vec x, int kx, Vec y, int ky, boolean floor) {
        if (x.bv == null && y.bv == null) {
            long a = x.lv[kx];
            long b = y.lv[ky];
            long q = Math.floorDiv(a, b);
            if (!floor) {
                long r = a - q * b;
                if (2 * Math.abs(r) > Math.abs(b)) q = q + 1;
            }
            qL = q;
            qB = null;
        } else {
            BigInteger a = x.bval(kx);
            BigInteger b = y.bval(ky);
            BigInteger[] qr = a.divideAndRemainder(b);
            BigInteger q = qr[0];
            BigInteger r = qr[1];
            if (floor) {
                if (r.signum() != 0 && r.signum() != b.signum()) q = q.subtract(BigInteger.ONE);
            } else {
                if (r.abs().shiftLeft(1).compareTo(b.abs()) > 0)
                    q = q.add(BigInteger.valueOf((long) (a.signum() * b.signum())));
            }
            if (q.bitLength() <= 62) {
                qL = q.longValue();
                qB = null;
            } else {
                qB = q;
            }
        }
    }

    /** x - q y for the current quotient q */
    private Vec subQ(Vec x, Vec y) {
        return qB == null ? axpy(x, -qL, y) : axpy(x, qB.negate(), y);
    }

    /**
     * Row operation  row i -= q row j  on A and U, for the current quotient q.
     *
     * The inverse of the left transformation changes by  column j += q column i.
     * With inverse == true this is recorded, and the caller has to apply all
     * recorded updates of column j at once with  Wt[j] = combine(Wt[j], Wt).
     * With inverse == false the inverse is not kept up to date (see hermite).
     */
    private void rowOp(int i, int j, boolean inverse) {
        U[i] = subQ(U[i], U[j]);
        setRow(i, subQ(A[i], A[j]));
        if (inverse) record(i);
        rowOps++;
        poll();
    }

    /** Records the term  q src[index]  of a pending update, q being the current quotient. */
    private void record(int index) {
        if (batchN == batchIdx.length) {
            int len = 2 * batchN;
            batchIdx = Arrays.copyOf(batchIdx, len);
            batchQL = Arrays.copyOf(batchQL, len);
            batchQB = Arrays.copyOf(batchQB, len);
        }
        batchIdx[batchN] = index;
        batchQL[batchN] = qL;
        batchQB[batchN] = qB;
        batchN++;
    }

    private void forgetRecorded() {
        for (int t = 0; t < batchN; t++) batchQB[t] = null;
        batchN = 0;
    }

    /**
     * Returns  base + sum_k q_k src[index_k]  over the recorded terms and forgets
     * them. One merge of all terms, because adding them one after the other would
     * take time quadratic in their number.
     */
    private Vec combine(Vec base, Vec[] src) {
        int k = batchN;
        batchN = 0;
        if (k == 0) return base;
        if (k <= 3) {
            Vec x = base;
            for (int t = 0; t < k; t++) {
                Vec y = src[batchIdx[t]];
                x = batchQB[t] == null ? axpy(x, batchQL[t], y) : axpy(x, batchQB[t], y);
                batchQB[t] = null;
            }
            return x;
        }
        long total = base.n;
        boolean big = base.bv != null;
        for (int t = 0; t < k; t++) {
            Vec y = src[batchIdx[t]];
            total += y.n;
            if (y.bv != null || batchQB[t] != null) big = true;
        }
        if (total > 2147483631L) throw new IllegalStateException("a vector has more than 2^31 entries");
        int len = (int) total;
        Vec result = big ? null : combineLong(base, src, k, len);
        if (result == null) result = combineBig(base, src, k, len);
        for (int t = 0; t < k; t++) batchQB[t] = null;
        return result;
    }

    private Vec combineLong(Vec base, Vec[] src, int k, int len) {
        long[] keys = new long[len];
        long[] vals = new long[len];
        int p = 0;
        for (int s = 0; s < base.n; s++) {
            keys[p] = ((long) base.idx[s] << 32) | (long) p;
            vals[p++] = base.lv[s];
        }
        try {
            for (int t = 0; t < k; t++) {
                Vec y = src[batchIdx[t]];
                long q = batchQL[t];
                int[] yi = y.idx;
                long[] yv = y.lv;
                for (int s = 0; s < y.n; s++) {
                    keys[p] = ((long) yi[s] << 32) | (long) p;
                    vals[p++] = Math.multiplyExact(q, yv[s]);
                }
            }
            Arrays.sort(keys);
            int[] ri = new int[len];
            long[] rv = new long[len];
            int cnt = 0;
            int s = 0;
            while (s < len) {
                int index = (int) (keys[s] >>> 32);
                long sum = vals[(int) keys[s]];
                s++;
                while (s < len && (int) (keys[s] >>> 32) == index) {
                    sum = Math.addExact(sum, vals[(int) keys[s]]);
                    s++;
                }
                if (sum > LIM || sum < -LIM) return null;
                if (sum != 0) {
                    ri[cnt] = index;
                    rv[cnt++] = sum;
                }
            }
            return makeLong(cnt, ri, rv);
        } catch (ArithmeticException e) {
            return null;
        }
    }

    private Vec combineBig(Vec base, Vec[] src, int k, int len) {
        long[] keys = new long[len];
        BigInteger[] vals = new BigInteger[len];
        int p = 0;
        for (int s = 0; s < base.n; s++) {
            keys[p] = ((long) base.idx[s] << 32) | (long) p;
            vals[p++] = base.bval(s);
        }
        for (int t = 0; t < k; t++) {
            Vec y = src[batchIdx[t]];
            BigInteger q = batchQB[t] != null ? batchQB[t] : BigInteger.valueOf(batchQL[t]);
            for (int s = 0; s < y.n; s++) {
                keys[p] = ((long) y.idx[s] << 32) | (long) p;
                vals[p++] = q.multiply(y.bval(s));
            }
        }
        Arrays.sort(keys);
        int[] ri = new int[len];
        BigInteger[] rv = new BigInteger[len];
        int cnt = 0;
        int s = 0;
        while (s < len) {
            int index = (int) (keys[s] >>> 32);
            BigInteger sum = vals[(int) keys[s]];
            s++;
            while (s < len && (int) (keys[s] >>> 32) == index) {
                sum = sum.add(vals[(int) keys[s]]);
                s++;
            }
            if (sum.signum() != 0) {
                ri[cnt] = index;
                rv[cnt++] = sum;
            }
        }
        return makeBig(cnt, ri, rv);
    }

    /* ====================================================================== */
    /*  Choice of pivots                                                       */
    /* ====================================================================== */

    private void push(int i) {
        Vec r = A[i];
        if (r.n == 0 || done[i]) return;
        if ((long) pq.size() > 4L * m + 1024L) {
            rebuildQueue();
            return;
        }
        pq.add(entry(i));
    }

    private Ent entry(int i) {
        Vec r = A[i];
        if (r.bv == null) {
            long best = Long.MAX_VALUE;
            long[] lv = r.lv;
            for (int k = 0; k < r.n; k++) {
                long a = lv[k] < 0 ? -lv[k] : lv[k];
                if (a < best) best = a;
            }
            return new Ent(best, null, weight(i), i, ver[i]);
        }
        BigInteger best = null;
        for (int k = 0; k < r.n; k++) {
            BigInteger a = r.bv[k].abs();
            if (best == null || a.compareTo(best) < 0) best = a;
        }
        if (best.bitLength() <= 62) return new Ent(best.longValue(), null, weight(i), i, ver[i]);
        return new Ent(Long.MAX_VALUE, best, weight(i), i, ver[i]);
    }

    private int weight(int i) {
        return A[i].n + U[i].n;
    }

    private void rebuildQueue() {
        pq.clear();
        for (int i = 0; i < m; i++) {
            if (A[i].n > 0 && !done[i]) pq.add(entry(i));
        }
    }

    private Ent pollValid() {
        while (true) {
            Ent e = pq.poll();
            if (e == null) return null;
            if (e.ver == ver[e.row] && !done[e.row] && A[e.row].n > 0) return e;
        }
    }

    /**
     * Among the entries of the row of e whose absolute value is the key of e:
     * the position with the smallest estimate of the work of a pivot step there.
     * The estimate is left in lastCost.
     */
    private int bestPosition(Ent e) {
        Vec r = A[e.row];
        long w = (long) (r.n - 1 + U[e.row].n);
        int best = -1;
        long bestCost = Long.MAX_VALUE;
        for (int k = 0; k < r.n; k++) {
            boolean match;
            if (e.bkey == null) {
                if (r.bv == null) match = (r.lv[k] < 0 ? -r.lv[k] : r.lv[k]) == e.key;
                else match = r.bv[k].bitLength() <= 62 && Math.abs(r.bv[k].longValue()) == e.key;
            } else {
                match = r.bval(k).abs().equals(e.bkey);
            }
            if (!match) continue;
            int c = r.idx[k];
            long cost = w * (long) (colCnt[c] - 1);
            if (Vt != null) cost += (long) (r.n - 1) * (long) Vt[c].n;
            if (cost < bestCost) {
                bestCost = cost;
                best = k;
            }
        }
        lastCost = bestCost;
        return best;
    }

    /**
     * The elimination. Pivot: an entry of smallest absolute value; among up to
     * CANDIDATES rows that have such an entry, the one with the smallest work
     * estimate. With rowsOnly (Hermite) a pivot row is set aside when its column
     * is clear; otherwise (Smith) its row is cleared as well, by column operations.
     */
    private void eliminate(boolean rowsOnly) {
        pivRow = new IntList();
        pivCol = new IntList();
        pq = new PriorityQueue<Ent>(Math.max(16, m));
        usePQ = true;
        rebuildQueue();
        Ent[] held = new Ent[CANDIDATES];
        while (true) {
            Ent best = pollValid();
            if (best == null) break;
            int pos = bestPosition(best);
            long cost = lastCost;
            int nh = 0;
            while (cost > 0 && nh < CANDIDATES - 1) {
                Ent e = pollValid();
                if (e == null) break;
                if (!e.sameKey(best)) {
                    held[nh++] = e;
                    break;
                }
                int p = bestPosition(e);
                if (lastCost < cost) {
                    held[nh++] = best;
                    best = e;
                    pos = p;
                    cost = lastCost;
                } else {
                    held[nh++] = e;
                }
            }
            for (int t = 0; t < nh; t++) pq.add(held[t]);
            pivotStep(best.row, A[best.row].idx[pos], rowsOnly);
        }
        usePQ = false;
        pq = null;
    }

    private void pivotStep(int i, int j, boolean rowsOnly) {
        Vec piv = A[i];
        int kp = piv.find(j);
        int[] col = column(j);
        boolean clean = true;
        for (int t = 0; t < col.length; t++) {
            int r = col[t];
            if (r == i) continue;
            Vec row = A[r];
            int kr = row.find(j);
            if (kr < 0) continue;
            quotient(row, kr, piv, kp, false);
            if (!qIsZero()) rowOp(r, i, true);
            if (A[r].find(j) >= 0) clean = false;
        }
        if (clean && rowsOnly) {
            /*
             * Row i is set aside and takes no part in the rest of the elimination.
             * Its column of the inverse is computed at the end of hermite().
             */
            forgetRecorded();
            Wt[i] = null;
            done[i] = true;
            for (int k = 0; k < piv.n; k++) colCnt[piv.idx[k]]--;
            pivRow.add(i);
            pivCol.add(j);
            return;
        }
        Wt[i] = combine(Wt[i], Wt);
        if (!clean) {
            /* a remainder is left in the column: it is smaller than the pivot and will be chosen next */
            push(i);
            return;
        }
        if (piv.n > 1) {
            /*
             * Column j has no other entry than the pivot, so the column operation
             * column c -= q column j  changes row i only. The inverse of the right
             * transformation changes by  row j += q row c.
             */
            int[] idx = new int[piv.n];
            BigInteger[] bv = piv.bv != null ? new BigInteger[piv.n] : null;
            long[] lv = piv.bv == null ? new long[piv.n] : null;
            int k = 0;
            for (int t = 0; t < piv.n; t++) {
                int c = piv.idx[t];
                if (t != kp) {
                    quotient(piv, t, piv, kp, false);
                    if (!qIsZero()) {
                        Vt[c] = subQ(Vt[c], Vt[j]);
                        record(c);
                        if (bv != null) {
                            BigInteger q = qB != null ? qB : BigInteger.valueOf(qL);
                            BigInteger v = piv.bv[t].subtract(q.multiply(piv.bv[kp]));
                            if (v.signum() != 0) {
                                idx[k] = c;
                                bv[k++] = v;
                            }
                        } else {
                            long v = piv.lv[t] - qL * piv.lv[kp];
                            if (v != 0) {
                                idx[k] = c;
                                lv[k++] = v;
                            }
                        }
                        poll();
                        continue;
                    }
                }
                idx[k] = c;
                if (bv != null) bv[k] = piv.bv[t];
                else lv[k] = piv.lv[t];
                k++;
            }
            Vi[j] = combine(Vi[j], Vi);
            setRow(i, bv != null ? makeBig(k, idx, bv) : makeLong(k, idx, lv));
        }
        if (A[i].n == 1) {
            pivRow.add(i);
            pivCol.add(j);
            pivVal = growBig(pivVal, pivRow.size);
            pivVal[pivRow.size - 1] = A[i].bval(0);
            setRow(i, ZERO);
        }
    }

    private static BigInteger[] growBig(BigInteger[] a, int size) {
        return size <= a.length ? a : Arrays.copyOf(a, Math.max(size, 2 * a.length));
    }

    /* ====================================================================== */
    /*  Smith normal form                                                      */
    /* ====================================================================== */

    /**
     * Computes u, u^-1, d, v, v^-1 with u.a.v == d for the matrix a that was set.
     * d is diagonal with non-negative entries, each dividing the next.
     * The results are read with cooRows etc. under the names "U", "UI", "D", "V", "VI".
     */
    public void smith() {
        requireMatrix();
        aborted = false;
        try {
            init(true);
            pivVal = new BigInteger[16];
            eliminate(false);
            int r = pivRow.size;
            BigInteger[] d = Arrays.copyOf(pivVal, r);
            for (int k = 0; k < r; k++) {
                if (d[k].signum() < 0) {
                    d[k] = d[k].negate();
                    int row = pivRow.a[k];
                    U[row] = neg(U[row]);
                    Wt[row] = neg(Wt[row]);
                }
            }
            int[] order = new int[r];
            int[] other = new int[r];
            int nOnes = 0, nOther = 0;
            for (int k = 0; k < r; k++) {
                if (d[k].equals(BigInteger.ONE)) order[nOnes++] = k;
                else other[nOther++] = k;
            }
            chain(d, other, nOther);
            for (int s = 0; s < nOther; s++) order[nOnes + s] = other[s];

            rank = r;
            resD = new Coo((long) r);
            for (int k = 0; k < r; k++) resD.add(k + 1, k + 1, d[order[k]]);

            int[] rowOrder = completeOrder(pivRow, order, m);
            resU = new Coo(totalSize(U));
            for (int i = 0; i < m; i++) {
                Vec v = U[rowOrder[i]];
                for (int k = 0; k < v.n; k++) resU.add(i + 1, v.idx[k] + 1, v, k);
                U[rowOrder[i]] = null;
            }
            resUI = new Coo(totalSize(Wt));
            for (int i = 0; i < m; i++) {
                Vec v = Wt[rowOrder[i]];
                for (int k = 0; k < v.n; k++) resUI.add(v.idx[k] + 1, i + 1, v, k);
                Wt[rowOrder[i]] = null;
            }
            int[] colOrder = completeOrder(pivCol, order, n);
            resV = new Coo(totalSize(Vt));
            for (int j = 0; j < n; j++) {
                Vec v = Vt[colOrder[j]];
                for (int k = 0; k < v.n; k++) resV.add(v.idx[k] + 1, j + 1, v, k);
                Vt[colOrder[j]] = null;
            }
            resVI = new Coo(totalSize(Vi));
            for (int j = 0; j < n; j++) {
                Vec v = Vi[colOrder[j]];
                for (int k = 0; k < v.n; k++) resVI.add(j + 1, v.idx[k] + 1, v, k);
                Vi[colOrder[j]] = null;
            }
        } catch (AbortedException e) {
            stopped();
        } finally {
            releaseWork();
        }
    }

    /**
     * Replaces the pivots number a and b, where d[a] does not divide d[b], by
     * their greatest common divisor and their least common multiple:
     *
     *   column ca += column cb
     *   (row ra, row rb) = (s row ra + t row rb, -(d[b]/g) row ra + (d[a]/g) row rb)
     *   column cb -= t (d[b]/g) column ca
     *
     * with g = s d[a] + t d[b].
     */
    private void gcdStep(BigInteger[] d, int a, int b) {
        BigInteger[] g = xgcd(d[a], d[b]);
        BigInteger alpha = d[a].divide(g[0]);
        BigInteger beta = d[b].divide(g[0]);
        int ra = pivRow.a[a], rb = pivRow.a[b];
        int ca = pivCol.a[a], cb = pivCol.a[b];

        Vt[ca] = axpy(Vt[ca], 1L, Vt[cb]);
        Vi[cb] = axpy(Vi[cb], -1L, Vi[ca]);

        Vec x = lincomb(g[1], U[ra], g[2], U[rb]);
        Vec y = lincomb(beta.negate(), U[ra], alpha, U[rb]);
        U[ra] = x;
        U[rb] = y;
        x = lincomb(alpha, Wt[ra], beta, Wt[rb]);
        y = lincomb(g[2].negate(), Wt[ra], g[1], Wt[rb]);
        Wt[ra] = x;
        Wt[rb] = y;

        BigInteger f = g[2].multiply(beta);
        Vt[cb] = axpy(Vt[cb], f.negate(), Vt[ca]);
        Vi[ca] = axpy(Vi[ca], f, Vi[cb]);

        d[b] = alpha.multiply(d[b]);
        d[a] = g[0];
        diagonalSteps++;
        poll();
    }

    /**
     * Puts the pivots other[0 .. nOther-1] (those that are not 1) in an order in
     * which each divides the next, changing them with gcdStep where necessary.
     *
     * The pivots are sorted by size and then added one by one to a chain
     * c[0] | c[1] | ... | c[t-1]. To add x, every c[i] that does not divide x is
     * replaced by gcd(c[i], x) and x by lcm(c[i], x), for i = 0, 1, ...; this
     * keeps the chain a chain and in the end everything divides x. The c[i] that
     * divide x form an initial piece of what is left of the chain, so the next
     * c[i] that does not divide x is found by bisection. If the sorted pivots
     * already form a chain, which is the usual case, this takes one division per
     * pivot and no operations on the matrices.
     */
    private void chain(BigInteger[] d, int[] other, int nOther) {
        if (nOther < 2) return;
        final BigInteger[] dd = d;
        Integer[] boxed = new Integer[nOther];
        for (int s = 0; s < nOther; s++) boxed[s] = other[s];
        Arrays.sort(boxed, new java.util.Comparator<Integer>() {
            @Override
            public int compare(Integer x, Integer y) {
                int c = dd[x].compareTo(dd[y]);
                return c != 0 ? c : x.compareTo(y);
            }
        });
        for (int s = 0; s < nOther; s++) other[s] = boxed[s];
        for (int t = 1; t < nOther; t++) {
            int b = other[t];
            int lo = 0;
            while (lo < t && d[b].mod(d[other[t - 1]]).signum() != 0) {
                /* the first i in lo .. t-1 for which d[other[i]] does not divide d[b] */
                int a = lo, z = t - 1;
                while (a < z) {
                    int mid = (a + z) >>> 1;
                    if (d[b].mod(d[other[mid]]).signum() == 0) a = mid + 1;
                    else z = mid;
                }
                gcdStep(d, other[a], b);
                lo = a + 1;
            }
            poll();
        }
    }

    /** The pivot rows (or columns) in the given order, followed by all others. */
    private static int[] completeOrder(IntList piv, int[] order, int size) {
        int[] res = new int[size];
        boolean[] used = new boolean[size];
        int k = 0;
        for (int t = 0; t < order.length; t++) {
            res[k] = piv.a[order[t]];
            used[res[k++]] = true;
        }
        for (int i = 0; i < size; i++) {
            if (!used[i]) res[k++] = i;
        }
        return res;
    }

    private static long totalSize(Vec[] v) {
        long s = 0;
        for (int i = 0; i < v.length; i++) s += v[i].n;
        return s;
    }

    /** { g, s, t } with g = s a + t b */
    static BigInteger[] xgcd(BigInteger a, BigInteger b) {
        BigInteger r0 = a, r1 = b;
        BigInteger s0 = BigInteger.ONE, s1 = BigInteger.ZERO;
        BigInteger t0 = BigInteger.ZERO, t1 = BigInteger.ONE;
        while (r1.signum() != 0) {
            BigInteger[] qr = r0.divideAndRemainder(r1);
            BigInteger tmp = r1;
            r1 = qr[1];
            r0 = tmp;
            tmp = s1;
            s1 = s0.subtract(qr[0].multiply(s1));
            s0 = tmp;
            tmp = t1;
            t1 = t0.subtract(qr[0].multiply(t1));
            t0 = tmp;
        }
        return new BigInteger[] { r0, s0, t0 };
    }

    /* ====================================================================== */
    /*  Hermite normal form                                                    */
    /* ====================================================================== */

    /**
     * Computes u, u^-1, h with u.a == h for the matrix a that was set. h is the
     * row Hermite normal form: row echelon form, positive pivots, the entries
     * above a pivot p in 0 .. p-1, zero rows at the bottom.
     * The results are read with cooRows etc. under the names "U", "UI", "H".
     *
     * The inverse of u is not carried through all the row operations. Its
     * columns that belong to the zero rows of h are final after the elimination
     * (step 1), because the later steps only combine the other rows. Its other
     * columns form the one matrix w with w.h' == a, where h' consists of the
     * nonzero rows of h; w is found at the end (step 6) by writing every row of
     * a as a combination of the rows of h, which is cheap because h is in
     * echelon form. Carrying these columns through steps 3 and 4 instead can
     * take several times longer than everything else together.
     */
    public void hermite() {
        requireMatrix();
        aborted = false;
        try {
            init(false);
            Vec[] input = A.clone();

            /* 1. row operations until rank many rows are left; these are independent */
            eliminate(true);
            int r = pivRow.size;

            /* 2. the column lists for the pivot rows only */
            Arrays.fill(colCnt, 0);
            for (int c = 0; c < n; c++) {
                if (colRows[c] != null) colRows[c].size = 0;
            }
            for (int k = 0; k < r; k++) {
                int row = pivRow.a[k];
                done[row] = false;
                Vec v = A[row];
                for (int t = 0; t < v.n; t++) {
                    colRows[v.idx[t]].add(row);
                    colCnt[v.idx[t]]++;
                }
            }

            /* 3. reduce the other pivot rows in the pivot columns, last pivot first */
            for (int k = r - 1; k >= 0; k--) {
                int row = pivRow.a[k];
                int col = pivCol.a[k];
                int[] rows = column(col);
                for (int t = 0; t < rows.length; t++) {
                    int o = rows[t];
                    if (o == row) continue;
                    int ko = A[o].find(col);
                    if (ko < 0) continue;
                    quotient(A[o], ko, A[row], A[row].find(col), false);
                    if (!qIsZero()) rowOp(o, row, false);
                }
            }

            /* 4. insert the rows, by first column, into a reduced echelon basis */
            owner = new int[n];
            Arrays.fill(owner, -1);
            long[] keys = new long[r];
            for (int k = 0; k < r; k++) keys[k] = ((long) A[pivRow.a[k]].idx[0] << 32) | (long) k;
            Arrays.sort(keys);
            for (int k = 0; k < r; k++) {
                int row = pivRow.a[(int) keys[k]];
                while (A[row].n > 0) {
                    int lead = A[row].idx[0];
                    int o = owner[lead];
                    if (o < 0) {
                        claim(row, lead);
                        break;
                    }
                    if (divides(A[o], A[row])) {
                        quotient(A[row], 0, A[o], 0, true);
                        rowOp(row, o, false);
                    } else {
                        /*
                         * (row o, row) = (s row o + t row, (b/g) row o - (a/g) row)
                         * with g = s a + t b the gcd of the two first entries a, b.
                         * The determinant of this operation is -1.
                         */
                        BigInteger a = A[o].bval(0);
                        BigInteger b = A[row].bval(0);
                        BigInteger[] g = xgcd(a, b.abs());
                        BigInteger s = g[1];
                        BigInteger t = b.signum() < 0 ? g[2].negate() : g[2];
                        BigInteger bg = b.divide(g[0]);
                        BigInteger agNeg = a.divide(g[0]).negate();
                        Vec x = lincomb(s, A[o], t, A[row]);
                        Vec y = lincomb(bg, A[o], agNeg, A[row]);
                        Vec ux = lincomb(s, U[o], t, U[row]);
                        Vec uy = lincomb(bg, U[o], agNeg, U[row]);
                        U[o] = ux;
                        U[row] = uy;
                        setRow(o, x);
                        setRow(row, y);
                        rowOps += 2;
                        poll();
                        claim(o, lead);
                    }
                }
            }

            /* 5. the rows in the order of their pivot columns */
            int[] order = new int[r];
            int cnt = 0;
            for (int c = 0; c < n; c++) {
                if (owner[c] >= 0) {
                    if (cnt < r) order[cnt] = owner[c];
                    cnt++;
                }
            }
            if (cnt != r) throw new IllegalStateException("internal error: rank mismatch in hermite");
            rank = r;
            long size = 0;
            for (int k = 0; k < r; k++) size += A[order[k]].n;
            resD = new Coo(size);
            for (int k = 0; k < r; k++) {
                Vec v = A[order[k]];
                for (int t = 0; t < v.n; t++) resD.add(k + 1, v.idx[t] + 1, v, t);
            }
            int[] rowOrder = new int[m];
            boolean[] used = new boolean[m];
            int c2 = 0;
            for (int k = 0; k < r; k++) {
                rowOrder[c2++] = order[k];
                used[order[k]] = true;
            }
            for (int i = 0; i < m; i++) {
                if (!used[i]) rowOrder[c2++] = i;
            }
            resU = new Coo(totalSize(U));
            for (int i = 0; i < m; i++) {
                Vec v = U[rowOrder[i]];
                for (int t = 0; t < v.n; t++) resU.add(i + 1, v.idx[t] + 1, v, t);
                U[rowOrder[i]] = null;
            }

            /* 6. the inverse of u */
            int[] position = new int[m];
            for (int i = 0; i < m; i++) position[rowOrder[i]] = i;
            Coo inverse = new Coo(Math.min(Coo.MAX, (long) inR.length + (long) m + 16L));
            for (int i = 0; i < m; i++) {
                Vec v = input[i];
                input[i] = null;
                while (v.n > 0) {
                    int lead = v.idx[0];
                    int o = owner[lead];
                    if (o < 0) throw notACombination();
                    quotient(v, 0, A[o], 0, true);
                    v = subQ(v, A[o]);
                    if (v.n > 0 && v.idx[0] <= lead) throw notACombination();
                    if (qB == null) inverse.add(i + 1, position[o] + 1, qL);
                    else inverse.add(i + 1, position[o] + 1, qB);
                    poll();
                }
            }
            for (int i = r; i < m; i++) {
                Vec v = Wt[rowOrder[i]];
                for (int t = 0; t < v.n; t++) inverse.add(v.idx[t] + 1, i + 1, v, t);
                Wt[rowOrder[i]] = null;
            }
            resUI = inverse;
        } catch (AbortedException e) {
            stopped();
        } finally {
            releaseWork();
        }
    }

    private static IllegalStateException notACombination() {
        return new IllegalStateException("internal error: a row is not a combination of the rows of h");
    }

    /**
     * Makes row the owner of column col: positive pivot, its entries in the pivot
     * columns of other rows reduced, and the entries of the other owners in
     * column col reduced.
     */
    private void claim(int row, int col) {
        owner[col] = row;
        boolean negative = A[row].bv != null ? A[row].bv[0].signum() < 0 : A[row].lv[0] < 0;
        if (negative) {
            setRow(row, neg(A[row]));
            U[row] = neg(U[row]);
        }
        reduceRow(row, col);
        int[] rows = column(col);
        for (int t = 0; t < rows.length; t++) {
            int o = rows[t];
            if (o != row && A[o].n > 0 && owner[A[o].idx[0]] == o) reduceRow(o, col - 1);
        }
    }

    /** Brings the entries of row that lie in pivot columns of other rows, right of column after, into 0 .. pivot-1. */
    private void reduceRow(int row, int after) {
        int c = after;
        while (true) {
            Vec v = A[row];
            int lo = 0, hi = v.n;
            while (lo < hi) {
                int mid = (lo + hi) >>> 1;
                if (v.idx[mid] <= c) lo = mid + 1;
                else hi = mid;
            }
            int k = lo;
            while (k < v.n && (owner[v.idx[k]] < 0 || owner[v.idx[k]] == row)) k++;
            if (k >= v.n) return;
            c = v.idx[k];
            int o = owner[c];
            quotient(v, k, A[o], 0, true);
            if (!qIsZero()) rowOp(row, o, false);
        }
    }

    /** Does the first entry of a divide the first entry of b? */
    private static boolean divides(Vec a, Vec b) {
        if (a.bv == null && b.bv == null) return b.lv[0] % a.lv[0] == 0;
        return b.bval(0).mod(a.bval(0).abs()).signum() == 0;
    }

    /* ====================================================================== */
    /*  Results                                                                */
    /* ====================================================================== */

    private void requireMatrix() {
        if (inR == null) throw new IllegalStateException("no matrix has been set");
    }

    private void clearResults() {
        rank = -1;
        resU = null;
        resUI = null;
        resD = null;
        resV = null;
        resVI = null;
    }

    private void releaseWork() {
        A = null;
        U = null;
        Wt = null;
        Vt = null;
        Vi = null;
        colRows = null;
        colCnt = null;
        ver = null;
        done = null;
        stamp = null;
        pq = null;
        usePQ = false;
        pivRow = null;
        pivCol = null;
        owner = null;
        batchN = 0;
        Arrays.fill(batchQB, null);
    }

    /** After an abort nothing is kept: no results, and the matrix has to be set again. */
    private void stopped() {
        clear();
        aborted = true;
    }

    /**
     * Was the last call of smith() or hermite() stopped before it was finished,
     * because the Mathematica evaluation was aborted or the thread interrupted?
     * Such a call returns normally and leaves no results.
     */
    public boolean wasAborted() {
        return aborted;
    }

    /** Forgets the matrix and the results, so that their memory can be reclaimed. */
    public void clear() {
        inR = null;
        inC = null;
        inL = null;
        inB = null;
        clearResults();
    }

    private Coo result(String which) {
        Coo c;
        if ("U".equals(which)) c = resU;
        else if ("UI".equals(which)) c = resUI;
        else if ("D".equals(which) || "H".equals(which)) c = resD;
        else if ("V".equals(which)) c = resV;
        else if ("VI".equals(which)) c = resVI;
        else throw new IllegalArgumentException("unknown matrix \"" + which + "\"");
        if (c == null) throw new IllegalStateException("matrix \"" + which + "\" has not been computed");
        return c;
    }

    /** The rank of the matrix; the number of nonzero entries of d, or of nonzero rows of h. */
    public int getRank() {
        if (rank < 0) throw new IllegalStateException("nothing has been computed");
        return rank;
    }

    public long getRowOperations() {
        return rowOps;
    }

    /** Number of pairs of diagonal entries that had to be replaced by their gcd and lcm (Smith). */
    public long getDiagonalSteps() {
        return diagonalSteps;
    }

    public boolean getUsedBigIntegers() {
        return usedBig;
    }

    /*
     * A result matrix is read as three lists of equal length: the row indices,
     * the column indices (both starting at 1) and the values of its nonzero
     * entries. which is "U", "UI", "D", "V" or "VI" after smith() and "U", "UI"
     * or "H" after hermite().
     */

    public int cooLength(String which) {
        return result(which).nnz;
    }

    public int[] cooRows(String which) {
        Coo c = result(which);
        return c.r.length == c.nnz ? c.r : Arrays.copyOf(c.r, c.nnz);
    }

    public int[] cooCols(String which) {
        Coo c = result(which);
        return c.c.length == c.nnz ? c.c : Arrays.copyOf(c.c, c.nnz);
    }

    /** Do all values fit 32 bit integers? Then cooValues can be used, else cooValuesString. */
    public boolean cooFitsInt(String which) {
        Coo c = result(which);
        if (c.bv != null) return false;
        for (int i = 0; i < c.nnz; i++) {
            if (c.lv[i] > 2147483647L || c.lv[i] < -2147483648L) return false;
        }
        return true;
    }

    public int[] cooValues(String which) {
        Coo c = result(which);
        if (!cooFitsInt(which)) throw new IllegalStateException("values do not fit 32 bit integers");
        int[] v = new int[c.nnz];
        for (int i = 0; i < c.nnz; i++) v[i] = (int) c.lv[i];
        return v;
    }

    /** The values as a string of the form {1,-2,3}. */
    public String cooValuesString(String which) {
        Coo c = result(which);
        StringBuilder sb = new StringBuilder((int) Math.min(1L << 30, Math.max(16L, 4L * c.nnz)));
        sb.append('{');
        for (int i = 0; i < c.nnz; i++) {
            if (i > 0) sb.append(',');
            if (c.bv != null) sb.append(c.bv[i].toString());
            else sb.append(c.lv[i]);
        }
        sb.append('}');
        return sb.toString();
    }

    /* ====================================================================== */
    /*  Abort                                                                  */
    /* ====================================================================== */

    /**
     * Every 4096 calls: leave the calculation if the thread was interrupted or
     * the Mathematica evaluation aborted. smith() and hermite() then return
     * normally, which is what J/Link expects of a method that notices an abort;
     * J/Link turns the return into an abort of the evaluation.
     */
    private void poll() {
        if ((++ticks & 4095) != 0) return;
        if (Thread.currentThread().isInterrupted()) throw new AbortedException();
        boolean stop = false;
        try {
            if (!jlinkLookedUp) {
                jlinkLookedUp = true;
                Class<?> stdLink = Class.forName("com.wolfram.jlink.StdLink");
                Class<?> kernelLink = Class.forName("com.wolfram.jlink.KernelLink");
                jlinkWasInterrupted = kernelLink.getMethod("wasInterrupted", new Class<?>[0]);
                jlinkGetLink = stdLink.getMethod("getLink", new Class<?>[0]);
            }
            if (jlinkGetLink != null) {
                Object link = jlinkGetLink.invoke(null, new Object[0]);
                if (link != null && Boolean.TRUE.equals(jlinkWasInterrupted.invoke(link, new Object[0])))
                    stop = true;
            }
        } catch (Throwable t) {
            jlinkGetLink = null;
        }
        if (stop) throw new AbortedException();
    }
}
