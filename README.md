# NER Tagger

A Maximum Entropy Sequence Model for [Named Entity Recognition (NER)](https://en.wikipedia.org/wiki/Named-entity_recognition) in Scala

Currently implements a trigram [Hidden Markov Model](https://en.wikipedia.org/wiki/Hidden_Markov_model)

Inference is done using the [Viterbi algorithm](https://en.wikipedia.org/wiki/Viterbi_algorithm)

## Build

A fully functional jar, including all dependencies, can be built using [sbt-assembly](https://github.com/sbt/sbt-assembly).

In the project's top level directory, run:

```$ sbt assembly```

## Usage

For a list of all available options, run the jar with `--help` option:

```sh
$ java -jar NERtagger.jar --help

  NER Tagger 1.0
  Usage: NERTagger [score|save|tag] [options] <args>...

    --help                   ouptut usage information
    -c, --corpus <str>       training corpus; must be one of {wiki|gene}
    -r, --rare <int>         rare count threshold
    -n, --ngram <int>        max n-gram size
    -t, --train <bool>       whether or not to retrain the model
    -p, --pp <str>           Rare word preprocessor; must be one of {pattern|replace}
  Command: score [options]
  Scores tagger on dev/key set and outputs tagged development set to file if provided.
    -o, --out <file>         output file name (optional)
  Command: save [options]
  Saves model rule counts to file for quicker loading
    -o, --out <file>         output file name
  Command: tag [options] <text>
  Tag sentence using trained model; output to file if provided
    <text>                   text to tag
    -o, --out <file>         output file name (optional)
```

## Example

```sh
$ java -jar NERtagger.jar tag 'Matthew Chamberlin lives in San Francisco , but his code lives on GitHub .'

  Using Wikipedia Documents
  Loading tagger...

  Output:
  Matthew|I-PER Chamberlin|I-PER lives|O in|O San|I-LOC Francisco|I-LOC ,|O but|O his|O code|O lives|O on|O GitHub|I-MISC .|O
```

## Provided Corpora

### Wikipedia NER Tags

The [Wikipedia Tag Corpus](http://schwa.org/projects/resources/wiki/Wikiner) from the Univeristy of Syndey's Schwa Lab.
Tag symbols are based on CoNLL named entity types.

Tag set:
* `I-MISC`
* `I-PER`
* `I-ORG`
* `I-LOC`
* `O`

### BioCreative GENE Tags

A slightly modified version of the MedTag/GENETAG corpus used in
the [BioCreative I challenge.](http://www.biocreative.org/tasks/biocreative-i/first-task-gm/)

Available in it's original form from the National Center for Biotechnology Information [here](ftp://ftp.ncbi.nlm.nih.gov/pub/lsmith/MedTag/).

Tag set:
* `I-GENE`
* `O`