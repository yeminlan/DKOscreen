#### Prepare #################################

## get list of samples
cd raw_data
ls *fastq.gz | sed "s|.fastq.gz||g" > samples
cd ..

## enumerate all combinations of guides
R --no-save < log/get_combinations.R

## build blast index with all combinations
cd raw_data/
makeblastdb -in combinations.fa -parse_seqids -dbtype nucl -out combinations
cd ..

#### TRIM ###################################

mkdir trim
for i in `cat raw_data/samples`;do
sed "s|example|$i|g" log/example.trim.batch > log/$i.trim.batch
bsub < log/$i.trim.batch
done

#### Blast ###################################

mkdir blast
for i in `cat raw_data/samples`;do
sed "s|example|$i|g" log/example.blast.batch > log/$i.blast.batch
bsub < log/$i.blast.batch
done

cd blast
R < log/blast.summary.R
cd ..

#### Summary #################################

R --no-save < log/summary.R

#### Statistics ##############################

R --no-save < log/filter.R
R --no-save < log/per.key.analysis.normal.R
R --no-save < log/per.gene.analysis.normal.R
R --no-save < log/figures.R
