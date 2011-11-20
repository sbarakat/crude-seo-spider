Crude SEO Spider
================

Provides a simple method of spidering a website to provide some basic url
information to assist in Search Engine Optimisation.

Features
--------

* Detects duplicate content using MD5 hashes
* Shows HTTP status codes for each url
* Displays the response time and page size
* Follows redirects
* Export results to CSV format
* Supports the Robots Exclusion Protocol (robots.txt)
* Supports rel="nofollow" link attribute

Usage
-----

For usage parameters run <pre>./spider.pl -h</pre>

1. First open and edit the spider.pl script and at the top set the full path to
the lib directory.

2. Modify the options in the spider.conf file, each option is commented so it
should be self explanatory.

3. Run the spider either by executing the script directly: <pre>./spider.pl</pre> Or by running the script through perl: <pre>perl spider.pl</pre>

4. While the script is running it will provide information on the currently
tracked urls and will be outputting the information to results.txt file.

Options
-------

To output to a CSV file provide the --csv=FILE perameter.

