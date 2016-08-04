package fiftycuatro.util;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class WrappingBitBuffer implements BitBuffer {

    private ByteOrder order = ByteOrder.BIG_ENDIAN;

    public static BitBuffer wrap(byte[] bytes) {
       return wrap(ByteBuffer.wrap(bytes));
    }

    public static BitBuffer wrap(ByteBuffer byteBuffer) {
        return new WrappingBitBuffer(byteBuffer);
    }

    public BitBuffer order(ByteOrder order) {
        this.order = order;
        return this;
    }

    private final ByteBuffer byteBuffer;
    private byte remainingBits = 0x0;
    private int remainingBitCount = 0;

    protected WrappingBitBuffer(ByteBuffer byteBuffer) {
       this.byteBuffer = byteBuffer;
    }

    private final static byte[] masks = { 0x00, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F, (byte)0xFF};

    private byte getByteInShort(Short s, int bitsInShort, int bitsToGet) {
        int shift = (bitsInShort > 8 ? 16 : 8) - (8 - bitsInShort) - bitsToGet;
        int mask = masks[bitsToGet];
        return (byte)((s >> shift) & mask);
    }

    @Override
    public byte getByte(int nBits) {
        short s;
        if (remainingBitCount == 0) {
            s = byteBuffer.get();
            remainingBitCount = 8;
        } else if (nBits <= remainingBitCount) {
            s = remainingBits;
        } else {
            s = (short)((remainingBits << 8) | byteBuffer.get());
            remainingBitCount += 8;
        }
        byte returnByte = getByteInShort(s, remainingBitCount, nBits);
        remainingBitCount -= nBits;
        remainingBits = (byte)(s & masks[remainingBitCount]);
        return returnByte;
    }

    @Override
    public int getInt(int nBits) {
        return getInt(nBits, this.order);
    }

    private final static byte[] signMasks = { (byte)0x80, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40};
    private final static byte[] twosCompMasks = { 0x7F, 0x00, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F};
    @Override
    public int getInt(int nBits, ByteOrder order) {
        byte[] bytes = getBytes(4, nBits, order);
        int bytesUsed = ((nBits - 1) / 8) + 1;
        int msbIdx = order == ByteOrder.BIG_ENDIAN ? 4 - bytesUsed : bytesUsed - 1;
        int bitloc = signMasks[(nBits % 8)];
        int sign = (bytes[msbIdx] & bitloc) == 0 ? 1 : -1;

        if (sign == -1) {
            // twos complement convert
            bytes[msbIdx] = (byte)((~bytes[msbIdx] & twosCompMasks[nBits % 8]) + 1);
        }

        return sign
                * ByteBuffer.wrap(bytes)
                .order(order)
                .getInt();
    }

    @Override
    public long getUnsignedInt(int nBits) {
        return getUnsignedInt(nBits, this.order);
    }

    @Override
    public long getUnsignedInt(int nBits, ByteOrder order) {
        return ByteBuffer.wrap(getBytes(4, nBits, order))
                .order(order)
                .getInt() & 0xFFFFFFFFl;
    }

    @Override
    public long getLong(int nBits) {
        return getLong(nBits, this.order);
    }

    @Override
    public long getLong(int nBits, ByteOrder order) {
        return ByteBuffer.wrap(getBytes(8, nBits, order))
                .order(order)
                .getLong();
    }

    // nBytes is how large of a byte array should be filled with 0
    // Endianess may not be fully thought out.
    // Bit endianess is always Big.
    private byte[] getBytes(int nBytes, int nBits, ByteOrder order) {
        byte[] bytes = new byte[nBytes];
        int bytesForBits = ((nBits -1) / 8) + 1;
        int idx = order == ByteOrder.BIG_ENDIAN ? 4 - bytesForBits : 0;

        for(int bitsLeft = nBits, i = idx; bitsLeft > 0; bitsLeft -= 8, i++) {
            int bitsToGet = bitsLeft % 8;
            bytes[i] = getByte(bitsToGet == 0 ? 8 : bitsToGet);
        }
        return bytes;
    }


    public void rewind() {
        remainingBitCount = 0;
        remainingBits = 0;
        byteBuffer.rewind();
    }

    private final String toStringFormat = "%s[pos=%d lim=%d cap=%d rem=%d cnt=%d]";

    public String toString() {
        return String.format(toStringFormat, WrappingBitBuffer.class.getCanonicalName(),
                byteBuffer.position(), byteBuffer.limit(), byteBuffer.capacity(), remainingBits, remainingBitCount);
    }
}
