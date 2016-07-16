package fiftycuatro.util;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class WrappingBitBuffer implements BitBuffer {

    private final ByteBuffer byteBuffer;
    private ByteOrder order = ByteOrder.BIG_ENDIAN;
    private byte remainingBits = 0x0;
    private int remainingBitCount = 0;

    private final static byte[] masks = { 0x00, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F, (byte)0xFF};
    private final static byte[] signMasks = { (byte)0x80, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40};
    private final static byte[] twosCompMasks = { 0x00, (byte)0xFE, (byte)0xFC, (byte)0xF8, (byte)0xF0, (byte)0xE0, (byte)0xC0, (byte)0x80 };

    public static BitBuffer wrap(byte[] bytes) {
       return wrap(ByteBuffer.wrap(bytes));
    }
    public static BitBuffer wrap(ByteBuffer byteBuffer) {
        return new WrappingBitBuffer(byteBuffer);
    }

    protected WrappingBitBuffer(ByteBuffer byteBuffer) {
        this.byteBuffer = byteBuffer;
    }

    public BitBuffer order(ByteOrder order) {
        this.order = order;
        return this;
    }

    private byte getByteInShort(Short s, int bitsInShort, int bitsToGet) {
        int shift = (bitsInShort > 8 ? 16 : 8) - (8 - bitsInShort) - bitsToGet;
        return (byte)((s >> shift) & masks[bitsToGet]);
    }

    /**
     * Main building block for this class.
     */
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

    /** Helper function for getting an array of bytes with the MSB
     *  correctly zeroized depending on the number of bits and
     *  endianess.
     *
     * nBytes is how large of a byte array should returned with
     * Most significant bytes/bits filled with 0. This is useful
     * when you know or care about what type you are decoding.
     *
     * Endianess may not be fully thought out but bit endianess
     * is always Big.
     */
    private byte[] getBytes(int nBytes, int nBits, boolean isSigned, ByteOrder order) {
        byte[] bytes = new byte[nBytes];
        int bytesForBits = ((nBits - 1) / 8) + 1;
        int startIndex = order == ByteOrder.BIG_ENDIAN ? 4 - bytesForBits : 0;

        for(int bitsLeft = nBits, i = startIndex; bitsLeft > 0; bitsLeft -= 8, i++) {
            int bitsToGet = bitsLeft % 8;
            bytes[i] = getByte(bitsToGet == 0 ? 8 : bitsToGet);
        }

        // Do conversion for signed numbers
        if (isSigned) {
            int msbIndex = order == ByteOrder.BIG_ENDIAN ? startIndex : bytesForBits - 1;

            // Only have to worry about items that have
            // most significant bit set
            if ((bytes[msbIndex] & signMasks[nBits % 8]) != 0) {
                bytes[msbIndex] = (byte)(bytes[msbIndex] | twosCompMasks[nBits % 8]);
                if ((bytes[msbIndex] & signMasks[nBits % 8]) != 0) {
                    int start = order == ByteOrder.BIG_ENDIAN ? 0 : bytesForBits;
                    int end = order == ByteOrder.BIG_ENDIAN ? startIndex : nBytes;
                    for (int i = start; i < end; i++) {
                        bytes[i] = (byte) 0xFF;
                    }
                }
            }
        }
        return bytes;
    }

    @Override
    public int getInt(int nBits) {
        return getInt(nBits, this.order);
    }

    @Override
    public int getInt(int nBits, ByteOrder order) {
        return ByteBuffer.wrap(getBytes(4, nBits, true, order))
                .order(order)
                .getInt();
    }

    @Override
    public long getUnsignedInt(int nBits) {
        return getUnsignedInt(nBits, this.order);
    }

    @Override
    public long getUnsignedInt(int nBits, ByteOrder order) {
        return ByteBuffer.wrap(getBytes(4, nBits, false, order))
                .order(order)
                .getInt() & 0xFFFFFFFFl;
    }

    @Override
    public long getLong(int nBits) {
        return getLong(nBits, this.order);
    }

    @Override
    public long getLong(int nBits, ByteOrder order) {
        byte[] bytes = getBytes(8, nBits, true, order);
        return ByteBuffer.wrap(getBytes(8, nBits, true, order))
                .order(order)
                .getLong();
    }


    public void rewind() {
        remainingBitCount = 0;
        remainingBits = 0;
        byteBuffer.rewind();
    }

    public String toString() {
        return String.format("%s[pos=%d lim=%d cap=%d rem=%d cnt=%d]",
                WrappingBitBuffer.class.getCanonicalName(),
                byteBuffer.position(),
                byteBuffer.limit(),
                byteBuffer.capacity(),
                remainingBits,
                remainingBitCount);
    }
}
