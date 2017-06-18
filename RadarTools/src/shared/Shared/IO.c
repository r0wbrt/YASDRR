/**
Copyright 2017 Robert Christian Taylor

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

/**
 * @file Routines for converting values to various types. Called from IO.hs.
 * @author Robert Christian Taylor
 * @license Apache-2.0
 * @copyright Robert Christian Taylor
 */

#include <stdint.h>
#include <string.h>

/**
 * Converts an array of signed 16 values to floats.
 * @param source The array of signed 16 values to convert to floats.
 * @param dest The memory location to write the conversion results to.
 * @param size The size of the int16_t array.
 */
void convertSigned16ArrayToFloatArray(int16_t * source, float * dest, int size) 
{
    int i;
    for(i = 0; i < size; i++) 
    {
        dest[i] = (float) source[i] / 32768.0;
    }
}

/**
 * Converts a double array into a float array.
 * @param source Pointer to the array of floats to copy in.
 * @param dest Pointer to the array to write floats into.
 * @param The size of the float array.
 */
void convertDoubleArrayToFloatArray(double * source, float * dest, int size)
{
    int i;
    for(i = 0; i < size; i++) 
    {
        dest[i] = (float)source[i];
    }
}

/**
 * Converts a complex float array into a complex double array.
 * @param source Pointer to the array of floats to read from.
 * @param dest Pointer to the array of doubles to write values to.
 * @param size The size of the array.
 */
void convertComplexFloatArrayToComplexDoubleArray(float * source, double * dest, int size)
{
    int i;
    for(i = 0; i < 2*size; i++)
    {
        dest[i] = (double)source[i];
    }
}

/**
 * Converts a complex float array to a complex signed 16 array.
 * @param source Pointer to complex array to read from.
 * @param dest Pointer to the array to write the signed 16 values to.
 * @param size The size of the array.
 */
void convertComplexFloatArrayToComplexSigned16(float * source, int16_t * dest, int size) 
{
    int i;
    for(i = 0; i < 2*size; i++)
    {
        float result = source[i] * 32767.0;
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

/**
 * Duplicates a complex float array. 
 * @param source The address to copy from.
 * @param dest The address to copy to.
 * @param size The size of the array in complex float.
 */
void convertComplexFloatArrayToComplexFloatArray(float * source, float * dest, int size)
{
    memcpy(dest, source, sizeof(float)*size*2);
}

/**
 * Duplicates a float array
 * @param source The address to copy from.
 * @param dest The address to copy to.
 * @param size The size of the array in floats.
 */
void convertFloatArrayToFloatArray(float * source, float * dest, int size)
{
    memcpy(dest, source, sizeof(float)*size);
}

/**
 * Converts a complex float array to its double magnitude squared (|z|^2).
 * @param source The source array.
 * @param dest The destination array to write the magnitudes to.
 * @param size The size of the array.
 */
void convertComplexFloatArrayToDoubleMag(float * source, double * dest, int size) 
{
    int i;
    for(i = 0; i < size; i++)
    {
        dest[i] = source[2*i]*source[2*i] + source[2*i + 1]*source[2*i + 1];
    }
}

/**
 * Converts a complex float array to its magnitude squared (|z|^2) stored 
 * as floating values.
 * @param source The source array.
 * @param dest The destination array to write the magnitudes into.
 * @param size The size of the array.
 */
void convertComplexFloatArrayToFloatMag(float * source, float * dest, int size)
{
    int i;
    for(i = 0; i < size; i++)
    {
        dest[i] = (float) (source[2*i]*source[2*i] + source[2*i + 1]*source[2*i + 1]);
    }
}

/**
 * Converts a complex float array to an array of the complex number's magnitudes 
 * stored as signed 16 numbers.
 * @param source The location to read from.
 * @param dest The location to write to.
 * @param size The size of the array.
 */
void convertComplexFloatArrayToSigned16Mag(float * source, int16_t * dest, int size)
{
    int i;
    for(i = 0; i < size; i++)
    {
        float result = (source[2*i]*source[2*i] + source[2*i + 1]*source[2*i + 1]) * 32767.0;
        
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
