#include <stdint.h>
#include <string.h>

/**
 * Converts an array of signed 16 values to doubles.
 * @param source The array of signed 16 values to convert to doubles.
 * @param dest The memory location to write the conversion results to.
 * @param size The size of the int16_t array.
 */
void convertSigned16ArrayToDoubleArray(int16_t * source, double * dest, int size) 
{
    int i;
    for(i = 0; i < size; i++) 
    {
        dest[i] = (double) source[i] / 32768.0;
    }
}

void convertFloatArrayToDoubleArray(float * source, double * dest, int size)
{
    int i;
    for(i = 0; i < size; i++) 
    {
        dest[i] = (float)source[i];
    }
}


void convertComplexDoubleArrayToComplexFloatArray(double * source, float * dest, int size)
{
    int i;
    for(i = 0; i < 2*size; i++)
    {
        dest[i] = (float)source[i];
    }
}

void convertComplexDoubleArrayToComplexSigned16(double * source, int16_t * dest, int size) 
{
    int i;
    for(i = 0; i < 2*size; i++)
    {
        double result = source[i] * 32767.0;
        if(result > 32767.0) 
        {
            dest[i] = 32767;
        } 
        else if (result < -32767.0) 
        {
            dest[i] = -32767;
        } 
        else
        {
            dest[i] = (int16_t)result;
        }
    }
}

void convertComplexDoubleArrayToComplexDoubleArray(double * source, double * dest, int size)
{
    memcpy(dest, source, sizeof(double)*size*2);
}


void convertComplexDoubleArrayToDoubleMag(double * source, double * dest, int size) 
{
    int i;
    for(i = 0; i < size; i++)
    {
        dest[i] = source[2*i]*source[2*i] + source[2*i + 1]*source[2*i + 1];
    }
}

void convertComplexDoubleArrayToFloatMag(double * source, float * dest, int size)
{
    int i;
    for(i = 0; i < size; i++)
    {
        dest[i] = (float) (source[2*i]*source[2*i] + source[2*i + 1]*source[2*i + 1]);
    }
}

void convertComplexDoubleArrayToSigned16Mag(double * source, int16_t * dest, int size)
{
    int i;
    for(i = 0; i < size; i++)
    {
        double result = (source[2*i]*source[2*i] + source[2*i + 1]*source[2*i + 1]) * 32767.0;
        
        if(result > 32767.0) 
        {
            dest[i] = 32767;
        } 
        else if (result < -32767.0) 
        {
            dest[i] = -32767;
        } 
        else
        {
            dest[i] = (int16_t)result;
        }
    }
}
