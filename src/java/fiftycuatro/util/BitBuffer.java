package fiftycuatro.util;

import java.nio.ByteOrder;

public interface BitBuffer {

    BitBuffer order(ByteOrder order);

    byte getByte(int nBits);

    int getInt();
    int getInt(ByteOrder order);
    int getInt(int nBits);
    int getInt(int nBits, ByteOrder order);

    long getLong();
    long getLong(ByteOrder order);
    long getLong(int nBits);
    long getLong(int nBits, ByteOrder order);
}
