#!/usr/bin/perl

$in_dir   = "raw";
$out_dir  = "preA";

$info_file = "$out_dir.info";

$doIdentifiers = 1;
$doComments    = 1;
$doStemming    = 1;
$doTokenize    = 1;
$doLineNumbers = 0;
$doRemoveSmallWords = 1;
$doRemoveSmall = 1;

$doStopwordsEnglish = 1;
$doStopwordsKeywords = 1;
$doStopwordsCustom = 1;

$numThreads = 4;

1;

