# NHS2 // Expansion of Natural History of Song Discography

## admin folder 

### Initial Item Sourcing
- **NHS2_NHS1ItemsOfInterest**: This spreadsheet contains a track information for possible tracks that could be useful. This was S.A.M. and M.S.'s first round of item searching using the Harvard Archive of World Music. You can find a record of each item via the Harvard Library at http://hollis.harvard.edu, doing an advanced search by Hollis #, and pasting the number in Column E.

- **NHS2_itemsOfInterest_questions** & **NHS2_ItemsOfInterest_responses**: NHS2_ItemsOfInterest was originally a Google Form; `questions` lists the questions/prompts from that google form, and `responses` lists the answers logged in the google form by research assistants. Researchers used this form to enter songs they had found that could have potentially been included in the corpus. This includes an initial draft of the songs' metadata: their region of origin, language (glottocode), and song type. 
- **NHS2_glottolog_protocol.docx**: This document describes the protocol applied by research assistants at the stage of initially designating a glottocode per song.

- **NHS2_searchstatus**: This tracker was used to direct research assistant's attention to different world regions for their search for ItemsOfInterest.

- **NHS2_HRAFsubRegions-centralAmerica.png** & **NHS2_HRAFSubRegions.png** & **NHS2_HRAF_North_America**: Reference maps used for designating region of origin for the songs.

- **NHS2_initial_pool**: Downstream of **NHS2_ItemsOfInterest_responses**, this is where research assistants TK and AK reviewed the submitted ItemsOfInterest, and indicated (in column A) which items we should go about attempting to obtain. The songs marked as 1 in Column A were marked as such in order to include at least one song from each song type and language family/major language. Where many songs were available per song type*language, a random subset was selected. At this stage, songs were excluded if the language family or culture could not be identified, or the singers were not audible. 

### Obtaining Items

- **NHS2_corpusBuilding_corpus** & **NHS2_corpusBuilding_removedFromCorpus**: The NHS2_corpusBuilding google sheet was used to keep track of how we sought to obtain items of interest. This gsheet had two tabs: one called `corpus` that contains those items of interest that did eventually form the corpus (pre-validation), and one called `removedFromCorpus` that contains those items of interest that we did not manage to source (due to missing liner notes, not obtainable from libraries, etc.). The `corpus` tab has columns `widesample`,`deepsample`, `wideonly`, `deeponly`, `both` (cells populated binary 0/1) to mark which tracks were marked of interest to obtain as part of the corpus' "wide" and "deep" samples (see Methods of manuscript).

- **NHS2_problem_tracks**: During the process of obtaining the files, it emerged that there were a number of tracks in the initial pool that were duplicates, incorrectly sourced, or inaccessible. These are documented here.

- **NHS2_corpus_replacements**: This was a gsheet that detailed some extra candidate tracks to fill gaps left by `problem_tracks` to maintain the intended breadth of the corpus’ global representation. 

- **NHS2_sourcing_Lidya_**: These sheets detail how LY (Lidya) went about obtaining many of the items of interest, from their various sources. This includes what source the track could be obtained from, what format the item was in (CD, LP etc.), whether they were already ready to digitize, and for any CD purchases, tracking cost of obtaining items.

- **NHS2_Harvard_Library_re-sourcing**: This sheet was used to track our obtaining items via Harvard Loeb libraries.

- **NHS2_LP_H_library_info.csv**: This sheet was used to track more specific information for those items acquired through Harvard libraries for those items that were sourced specifically as LPs. Contains Harvard Hollis library reference numbers.

### Validation

- **NHS2_corpusValidationProtocol.docx**: This document describes the protocol used by research assistants at this stage of validating that the metadata and liner notes accompanying the obtained items in fact correspond to the metadata logged during the initial item search. This document additionally details the protocol research assistants followed to prepare and save the audio that would ultimately be included in the corpus.

- **NHS2_corpusValidation.csv**: This sheet contains the primary coders' (AM and JL) review of the songs' initial draft of metadata. They followed the validation protocol described above. 

- **NHS2_corpusDoubleValidation.csv**: This sheet contains the secondary coders'  (SO and VM) review of the songs' initial draft of metadata, following the same validation protocol as the primary coders, and unaware of the decisions made by the primary coders. 

- **NHS2_corpusSemiFinal.csv**: This sheet complied the information from both sets of coders (AM and JL; SO and VM) documented in the two above documents. A third researcher (MB or CH) attended to any songs for which there was any discrepancy between the two sets of coders; they revisited the source liner notes for these songs and made a final arbitration between the two sets of coders' decisions.
