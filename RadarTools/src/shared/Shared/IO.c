#include <stdint.h>

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


/**
 * Converts an array of */
void convertDoubleArrayToSigned16(double * source, int16_t * dest, int size) 
{
    int i;
    for(i = 0; i < size; i++)
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
