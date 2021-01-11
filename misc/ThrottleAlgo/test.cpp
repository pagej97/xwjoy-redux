#include "pch.h"
#include <cstdint>


#define BANDS 3
#define FINE_BANDS_PER_BAND 10

struct JSCalib
{
    JSCalib(uint16_t limitBottom, uint16_t limitTop):
        _limitBottom(limitBottom),
        _limitTop(limitTop)
    {
        computeDerivatives();
    }

    void computeDerivatives()
    {
        _invert = _limitBottom > _limitTop;
        uint16_t range = _invert ? (_limitBottom - _limitTop) : (_limitTop - _limitBottom);
        _bandWidth = range / BANDS;
        _fineBandWidth = _bandWidth / FINE_BANDS_PER_BAND;
    }

    bool _invert;
    uint16_t _limitBottom;
    uint16_t _limitTop;
    uint16_t _bandWidth;
    uint16_t _fineBandWidth;
};

struct BandOffset
{
    uint8_t _absoluteBand;
    uint8_t _offsetRelativeBand;
    int16_t _offset;
};

BandOffset getBandOffsetFromPosition(uint16_t position, JSCalib *cal, BandOffset *prev)
{
    BandOffset rv;

    uint16_t normalizedPos = position - cal->_limitBottom;

    rv._absoluteBand = normalizedPos / cal->_bandWidth;

    // determing whether we're falling relative to previous band
    bool falling;
    if (!prev)
        falling = false;
    else if (prev->_absoluteBand < rv._absoluteBand)
        falling = false; // moving to higher band
    else if (prev->_absoluteBand > rv._absoluteBand)
        falling = true; // moving to lower band
    else
        falling = (prev->_offset < 0); // keep previous state

    rv._offsetRelativeBand = (falling) ? (rv._absoluteBand + 1) : (rv._absoluteBand);

    int16_t offsetPos = normalizedPos - (rv._offsetRelativeBand * cal->_bandWidth);
    rv._offset = offsetPos / cal->_fineBandWidth;

    return rv;
}

class ThrottleAlgo: public ::testing::Test
{
public:

    virtual void SetUp() override
    {
    }

    virtual void TearDown() override
    {
    }


    unsigned char _currentBand;
    int _currentOffset; // signed offset within band

    std::string _result;
};

TEST_F(ThrottleAlgo, TestSimple_Positive)
{
    JSCalib cal(13, 247);

    BandOffset bo1 = getBandOffsetFromPosition(75, &cal, NULL);
    EXPECT_EQ(bo1._absoluteBand, 0);
    EXPECT_EQ(bo1._offsetRelativeBand, 0);
    EXPECT_EQ(bo1._offset, 8);

    BandOffset bo2 = getBandOffsetFromPosition(131, &cal, &bo1);
    EXPECT_EQ(bo2._absoluteBand, 1);
    EXPECT_EQ(bo2._offsetRelativeBand, 1);
    EXPECT_EQ(bo2._offset, 5);

    BandOffset bo3 = getBandOffsetFromPosition(194, &cal, &bo2);
    EXPECT_EQ(bo3._absoluteBand, 2);
    EXPECT_EQ(bo3._offsetRelativeBand, 2);
    EXPECT_EQ(bo3._offset, 3);
}

TEST_F(ThrottleAlgo, TestSimple_Negative)
{
    JSCalib cal(13, 247);

    BandOffset bo1 = getBandOffsetFromPosition(194, &cal, NULL);
    EXPECT_EQ(bo1._absoluteBand, 2);
    EXPECT_EQ(bo1._offsetRelativeBand, 2);
    EXPECT_EQ(bo1._offset, 3);

    // now we'll consider from the "falling" perspective
    ++bo1._offsetRelativeBand;
    bo1._offset = FINE_BANDS_PER_BAND - bo1._offset;

    BandOffset bo2 = getBandOffsetFromPosition(131, &cal, &bo1);
    EXPECT_EQ(bo2._absoluteBand, 1);
    EXPECT_EQ(bo2._offsetRelativeBand, 2);
    EXPECT_EQ(bo2._offset, -5);

    BandOffset bo3 = getBandOffsetFromPosition(75, &cal, &bo2);
    EXPECT_EQ(bo3._absoluteBand, 0);
    EXPECT_EQ(bo3._offsetRelativeBand, 1);
    EXPECT_EQ(bo3._offset, -2);
}

TEST_F(ThrottleAlgo, TestMovingIntraBand_Positive)
{
    JSCalib cal(13, 247);

    BandOffset bo1 = getBandOffsetFromPosition(75, &cal, NULL);
    BandOffset bo2 = getBandOffsetFromPosition(131, &cal, &bo1);

    BandOffset bo3 = getBandOffsetFromPosition(142, &cal, &bo2);
    EXPECT_EQ(bo3._absoluteBand, 1);
    EXPECT_EQ(bo3._offsetRelativeBand, 1);
    EXPECT_EQ(bo3._offset, 7);

    BandOffset bo4 = getBandOffsetFromPosition(117, &cal, &bo3);
    EXPECT_EQ(bo4._absoluteBand, 1);
    EXPECT_EQ(bo4._offsetRelativeBand, 1);
    EXPECT_EQ(bo4._offset, 3);
}

TEST_F(ThrottleAlgo, TestMovingIntraBand_Negative)
{
    JSCalib cal(13, 247);

    BandOffset bo1 = getBandOffsetFromPosition(194, &cal, NULL);
    BandOffset bo2 = getBandOffsetFromPosition(131, &cal, &bo1);

    BandOffset bo3 = getBandOffsetFromPosition(142, &cal, &bo2);
    EXPECT_EQ(bo3._absoluteBand, 1);
    EXPECT_EQ(bo3._offsetRelativeBand, 2);
    EXPECT_EQ(bo3._offset, -3);

    BandOffset bo4 = getBandOffsetFromPosition(117, &cal, &bo3);
    EXPECT_EQ(bo4._absoluteBand, 1);
    EXPECT_EQ(bo4._offsetRelativeBand, 2);
    EXPECT_EQ(bo4._offset, -7);
}

TEST_F(ThrottleAlgo, TestMovingInterBand_PositiveThenNegative)
{
    JSCalib cal(13, 247);

    BandOffset bo1 = getBandOffsetFromPosition(75, &cal, NULL);
    EXPECT_EQ(bo1._absoluteBand, 0);
    EXPECT_EQ(bo1._offsetRelativeBand, 0);
    EXPECT_EQ(bo1._offset, 8);

    BandOffset bo2 = getBandOffsetFromPosition(131, &cal, &bo1);
    EXPECT_EQ(bo2._absoluteBand, 1);
    EXPECT_EQ(bo2._offsetRelativeBand, 1);
    EXPECT_EQ(bo2._offset, 5);

    BandOffset bo3 = getBandOffsetFromPosition(75, &cal, &bo2);
    EXPECT_EQ(bo3._absoluteBand, bo1._absoluteBand);
    EXPECT_EQ(bo3._offsetRelativeBand, bo1._offsetRelativeBand + 1);
    EXPECT_EQ(bo3._offset, bo1._offset - FINE_BANDS_PER_BAND);
}

TEST_F(ThrottleAlgo, TestMovingInterBand_NegativeThenPositive)
{
    JSCalib cal(13, 247);

    BandOffset bo1 = getBandOffsetFromPosition(194, &cal, NULL);
    EXPECT_EQ(bo1._absoluteBand, 2);
    EXPECT_EQ(bo1._offsetRelativeBand, 2);
    EXPECT_EQ(bo1._offset, 3);

    BandOffset bo2 = getBandOffsetFromPosition(131, &cal, &bo1);
    EXPECT_EQ(bo2._absoluteBand, 1);
    EXPECT_EQ(bo2._offsetRelativeBand, 2);
    EXPECT_EQ(bo2._offset, -5);

    BandOffset bo3 = getBandOffsetFromPosition(194, &cal, &bo2);
    EXPECT_EQ(bo1._absoluteBand, 2);
    EXPECT_EQ(bo1._offsetRelativeBand, 2);
    EXPECT_EQ(bo1._offset, 3);
}
