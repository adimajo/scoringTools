## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(data.frame(Job=c("Craftsman","Technician","Executive","Office employee"),Habitation = c("Owner","Renter","Starter","By family"),Time_in_job = c(10,20,5,2), Children = c(0,1,2,3), Family_status=  c("Divorced","Widower","Single","Married"),Default = c("No","No","Yes","No")))

## ---- echo=FALSE, results='asis'-----------------------------------------
# scoring_model <- glm()

## ---- echo=FALSE, results='asis'-----------------------------------------
# scoring_model$coefficients

## ---- echo=FALSE, results='asis'-----------------------------------------
# scoring_model$LogLik

## ---- fig.show='hold'----------------------------------------------------
# plot(augmentation_model)

## ---- echo=FALSE, results='asis'-----------------------------------------
# fuzzy_model <- fuzzy_augmentation(xf,xnf,yf)

## ---- echo=FALSE, results='asis'-----------------------------------------
# reclassification_model <- reclassifcation(xf,xnf,yf)

## ---- echo=FALSE, results='asis'-----------------------------------------
# augmentation_model <- augmentation(xf,xnf,yf)

## ---- echo=FALSE, results='asis'-----------------------------------------
# parcelling_model <- parcelling(xf,xnf,yf,alpha)

## ---- echo=FALSE, results='asis'-----------------------------------------
# twins_model <- twins(xf,xnf,yf)

## ---- echo=FALSE, results='asis'-----------------------------------------
# augmentation_model@method
# parcelling_model@financed_model
# twins_model@acceptance_model
# reclassification_model@infered_model

## ---- echo=FALSE, results='asis'-----------------------------------------
# print(augmentation_model)
# print(glm())

## ---- echo=FALSE, results='asis'-----------------------------------------
# summary(augmentation_model)
# summary(glm())

## ---- echo=FALSE, results='asis'-----------------------------------------
# predict(augmentation_model)
# predict(augmentation_model@infered_model)

## ---- fig.show='hold'----------------------------------------------------
# plot(augmentation_model)

## ---- echo=FALSE, results='asis'-----------------------------------------
# sem_disc <- SEM_discretization()

## ---- echo=FALSE, results='asis'-----------------------------------------
# new_disc <- discretize(new_dataset,sem_disc)

## ---- echo=FALSE, results='asis'-----------------------------------------
# print(sem_disc)

## ---- echo=FALSE, results='asis'-----------------------------------------
# summary(sem_disc)

## ---- echo=FALSE, results='asis'-----------------------------------------
# predict(new_disc)

## ---- echo=FALSE, results='asis'-----------------------------------------
# plot(sem_disc,type="discretization"")

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

