# Code book for `Getting and Cleaning Data` Coursera course project.
This code book summarizes the tidy data fields in result.txt.

## Description for data fields
### First two fields are identifiers
- subject - The ID of the test subject
- activity - The type of activity performed when the corresponding measurements were taken
	* WALKING: subject was walking
	* WALKING_UPSTAIRS: subject was walking up a staircase
	* WALKING_DOWNSTAIRS: subject was walking down a staircase
	* SITTING: subject was sitting
	* STANDING: subject was standing
	* LAYING: subject was laying down

### Subsequence fields are average of measurements

The subsequence fields are average of measurements by subjects and activities.
The measurements come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
- tBodyAcc-XYZ
- tGravityAcc-XYZ
- tBodyAccJerk-XYZ
- tBodyGyro-XYZ
- tBodyGyroJerk-XYZ
- tBodyAccMag
- tGravityAccMag
- tBodyAccJerkMag
- tBodyGyroMag
- tBodyGyroJerkMag
- fBodyAcc-XYZ
- fBodyAccJerk-XYZ
- fBodyGyro-XYZ
- fBodyAccMag
- fBodyAccJerkMag
- fBodyGyroMag
- fBodyGyroJerkMag

Two variables that were estimated from these signals are: 

- mean(): Mean value
- std(): Standard deviation

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

- gravityMean
- tBodyAccMean
- tBodyAccJerkMean
- tBodyGyroMean
- tBodyGyroJerkMean

The complete list of fields show as following:

- Time-domain signals
	* tBodyAccMeanX
	* tBodyAccMeanY
	* tBodyAccMeanZ
	* tBodyAccStdX
	* tBodyAccStdY
	* tBodyAccStdZ
	* tGravityAccMeanX
	* tGravityAccMeanY
	* tGravityAccMeanZ
	* tGravityAccStdX
	* tGravityAccStdY
	* tGravityAccStdZ
	* tBodyAccJerkMeanX
	* tBodyAccJerkMeanY
	* tBodyAccJerkMeanZ
	* tBodyAccJerkStdX
	* tBodyAccJerkStdY
	* tBodyAccJerkStdZ
	* tBodyGyroMeanX
	* tBodyGyroMeanY
	* tBodyGyroMeanZ
	* tBodyGyroStdX
	* tBodyGyroStdY
	* tBodyGyroStdZ
	* tBodyGyroJerkMeanX
	* tBodyGyroJerkMeanY
	* tBodyGyroJerkMeanZ
	* tBodyGyroJerkStdX
	* tBodyGyroJerkStdY
	* tBodyGyroJerkStdZ
	* tBodyAccMagMean
	* tBodyAccMagStd
	* tGravityAccMagMean
	* tGravityAccMagStd
	* tBodyAccJerkMagMean
	* tBodyAccJerkMagStd
	* tBodyGyroMagMean
	* tBodyGyroMagStd
	* tBodyGyroJerkMagMean
	* tBodyGyroJerkMagStd

- Frequency-domain signals
	* fBodyAccMeanX
	* fBodyAccMeanY
	* fBodyAccMeanZ
	* fBodyAccStdX
	* fBodyAccStdY
	* fBodyAccStdZ
	* fBodyAccMeanFreqX
	* fBodyAccMeanFreqY
	* fBodyAccMeanFreqZ
	* fBodyAccJerkMeanX
	* fBodyAccJerkMeanY
	* fBodyAccJerkMeanZ
	* fBodyAccJerkStdX
	* fBodyAccJerkStdY
	* fBodyAccJerkStdZ
	* fBodyAccJerkMeanFreqX
	* fBodyAccJerkMeanFreqY
	* fBodyAccJerkMeanFreqZ
	* fBodyGyroMeanX
	* fBodyGyroMeanY
	* fBodyGyroMeanZ
	* fBodyGyroStdX
	* fBodyGyroStdY
	* fBodyGyroStdZ
	* fBodyGyroMeanFreqX
	* fBodyGyroMeanFreqY
	* fBodyGyroMeanFreqZ
	* fBodyAccMagMean
	* fBodyAccMagStd
	* fBodyAccMagMeanFreq
	* fBodyBodyAccJerkMagMean
	* fBodyBodyAccJerkMagStd
	* fBodyBodyAccJerkMagMeanFreq
	* fBodyBodyGyroMagMean
	* fBodyBodyGyroMagStd
	* fBodyBodyGyroMagMeanFreq
	* fBodyBodyGyroJerkMagMean
	* fBodyBodyGyroJerkMagStd
	* fBodyBodyGyroJerkMagMeanFreq

