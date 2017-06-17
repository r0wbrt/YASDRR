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

/**
 * Converts a float array into a double array.
 * @param source Pointer to the array of floats to copy in.
 * @param dest Pointer to the array to write doubles into.
 * @param The size of the float array.
 */
void convertFloatArrayToDoubleArray(float * source, double * dest, int size)
{
    int i;
    for(i = 0; i < size; i++) 
    {
        dest[i] = (double)source[i];
    }
}

/**
 * Converts a complex double array into a complex float array.
 * @param source Pointer to the array of doubles to read from.
 * @param dest Pointer to the array of floats to write values to.
 * @param size The size of the array.
 */
void convertComplexDoubleArrayToComplexFloatArray(double * source, float * dest, int size)
{
    int i;
    for(i = 0; i < 2*size; i++)
    {
        dest[i] = (float)source[i];
    }
}

/**
 * Converts a complex double array to a complex signed 16 array.
 * @param source Pointer to complex array to read from.
 * @param dest Pointer to the array to write the signed 16 values to.
 * @param size The size of the array.
 */
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

/**
 * Duplicates a complex double array. 
 * @param source The address to copy from.
 * @param dest The address to copy to.
 * @param size The size of the array in complex doubles.
 */
void convertComplexDoubleArrayToComplexDoubleArray(double * source, double * dest, int size)
{
    memcpy(dest, source, sizeof(double)*size*2);
}

/**
 * Duplicates a double array
 * @param source The address to copy from.
 * @param dest The address to copy to.
 * @param size The size of the array in doubles.
 */
void convertDoubleArrayToDoubleArray(double * source, double * dest, int size)
{
    memcpy(dest, source, sizeof(double)*size);
}

/**
 * Converts a complex double array to its magnitude squared (|z|^2).
 * @param source The source array.
 * @param dest The destination array to write the magnitudes to.
 * @param size The size of the array.
 */
void convertComplexDoubleArrayToDoubleMag(double * source, double * dest, int size) 
{
    int i;
    for(i = 0; i < size; i++)
    {
        dest[i] = source[2*i]*source[2*i] + source[2*i + 1]*source[2*i + 1];
    }
}

/**
 * Converts a complex double array to its magnitude squared (|z|^2) stored 
 * as floating values.
 * @param source The source array.
 * @param dest The destination array to write the magnitudes into.
 * @param size The size of the array.
 */
void convertComplexDoubleArrayToFloatMag(double * source, float * dest, int size)
{
    int i;
    for(i = 0; i < size; i++)
    {
        dest[i] = (float) (source[2*i]*source[2*i] + source[2*i + 1]*source[2*i + 1]);
    }
}

/**
 * Converts a complex double array to an array of the complex number's magnitudes 
 * stored as signed 16 numbers.
 * @param source The location to read from.
 * @param dest The location to write to.
 * @param size The size of the array.
 */
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
