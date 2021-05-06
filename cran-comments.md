# NOTE regarding Maintainer field

checking DESCRIPTION meta-information ... NOTE  
Maintainer field differs from that derived from Authors@R  
  Maintainer: ‘Tomasz \u017b\u00F3\u0142tak <tomek@zozlak.org>’  
  Authors@R:  ‘Tomasz Żółtak <tomek@zozlak.org>’

This is a false-positive: note comes from that in the text in Authors@R Unicode code points (that describe non-ASCII letters in my surname) have already been replaced by actual UTF-encoded characters while in the "Maintainer" field they are still represented as descriptions of code-points. Nevertheless this describe the same letters as those appearing in Authors@R.
