# MTF-PA
Data and R scripts for three studies on development of Motivation Thought frequency and State Motivation Scales for Physical Activity, published as:
Kavanagh, D.J., et al (2020) The Motivational Thought Frequency scales for increased physical activity and reduced high-energy snacking. British Journal of Health Psychology. doi: 10.1111/bjhp.12422

Study 1: The data file for Study 1 is MTFSMPA.csv. This contains five demographic variables, then 13 items from the MTF-PA and 13 from the SM-PA. Demographics variables include Site (1 = AUS; 2 = UK), Sex (1=male, 2=female), Age in years, Education () and Relationship. The Analysis script for Study 1 is Analysis_S1.R. It will send output to the files MTFAnalysis.txt and SMAnalysis.txt. Some output generating lines are commented out - to show full model specifications uncomment these. In the lines that compute reliability confidence intervals, note that you can reduce the bootstraps to 100 without much change to the values reported but with a huge saving in runtime.

Study 2: use script Analysis_S2.R and data file Study2.csv (MTF-PA)

Study 2S: Use script Analysis_S2S.R and data file Study2S.csv (MTF-S)
