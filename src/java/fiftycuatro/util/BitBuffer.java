package fiftycuatro.util;

import java.math.BigInteger;
import java.nio.ByteOrder;

public interface BitBuffer {

    BitBuffer order(ByteOrder order);

    byte getByte(int nBits);

    int getInt(int nBits);
    int getInt(int nBits, ByteOrder order);

    long getUnsignedInt(int nBits);
    long getUnsignedInt(int nBits, ByteOrder order);

    long getLong(int nBits);
    long getLong(int nBits, ByteOrder order);

    //BigInteger getUnsignedLong(int nBits);
    //BigInteger getUnsignedLong(int nBits, ByteOrder order);

    //BigInteger getBigInt(int nBits);
    //BigInteger getBigInt(int nBits, ByteOrder order);

    //BigInteger getUnsignedBigInt(int nBits);
    //BigInteger getUnsignedBigInt(int nBits, ByteOrder order);
}
