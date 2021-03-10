##################################################################
#########################   CNN model   ##########################

# Copying spectrum images to training,validation, and test directories
Original_dataset_dir<-"/Users/luzhang/Desktop/spectrum_logReturn/CMO"

base_dir<-"/Users/luzhang/Desktop/CMO"
dir.create(base_dir)

train_dir<-file.path(base_dir,"train")
dir.create(train_dir)
test_dir<-file.path(base_dir,"test")
dir.create(test_dir)

train_up_dir<-file.path(train_dir,"upspectrum")
dir.create(train_up_dir)

train_down_dir<-file.path(train_dir,"downspectrum")
dir.create(train_down_dir)


test_up_dir<-file.path(test_dir,"upspectrum")
dir.create(test_up_dir)

test_down_dir<-file.path(test_dir,"downspectrum")
dir.create(test_down_dir)

fnames<-paste0("Upsepctrum", 1:1000, ".png")
file.copy(file.path(Original_dataset_dir, fnames),
          file.path(train_up_dir))

fnames<-paste0("Upsepctrum", 1001:1534, ".png")
file.copy(file.path(Original_dataset_dir, fnames),
          file.path(test_up_dir))

fnames<-paste0("Downsepctrum", 1:1000, ".png")
file.copy(file.path(Original_dataset_dir, fnames),
          file.path(train_down_dir))

fnames<-paste0("Downsepctrum", 1001:1534, ".png")
file.copy(file.path(Original_dataset_dir, fnames),
          file.path(test_down_dir))

#check how many pictures are in each training split(train/validation/test)
cat("total training upspectrum images:", length(list.files(train_up_dir)),"\n")
cat("total training downspectrum images:", length(list.files(train_down_dir)),"\n")

cat("total test upspectrum images:", length(list.files(test_up_dir)),"\n")
cat("total test downspectrum images:", length(list.files(test_down_dir)),"\n")

# Building networks
rm(list = setdiff(ls(), lsf.str())) # don't run
library(keras)

model<-keras_model_sequential()%>%
  layer_conv_2d(filters=32, kernel_size=c(3,3),activation="relu",
                input_shape=c(150,150,3))%>%
  layer_max_pooling_2d(pool_size=c(2,2))%>%
  layer_conv_2d(filters=64, kernel_size=c(3,3),activation="relu",
                input_shape=c(150,150,3))%>%
  layer_max_pooling_2d(pool_size=c(2,2))%>%
  layer_conv_2d(filters=128, kernel_size=c(3,3),activation="relu",
                input_shape=c(150,150,3))%>%
  layer_max_pooling_2d(pool_size=c(2,2))%>%
  layer_flatten() %>%
  layer_dense(units=512, activation="relu")%>%
  layer_dense(units=1,activation="sigmoid")

summary(model)

# Configuring the model for training
model %>% compile(
  loss="binary_crossentropy",
  optimizer=optimizer_rmsprop(lr=1e-4),
  metrics=c("acc")
)

# using image_data_generator to read images from directions
train_datagen<-image_data_generator(rescale = 1/255)
validation_datagen<-image_data_generator(rescale=1/255)
test_datagen<-image_data_generator(rescale = 1/255)

train_generator<-flow_images_from_directory(
  train_dir,
  train_datagen,
  target_size = c(150,150),
  batch_size = 20,
  class_mode = "binary"
)

validation_generator<-flow_images_from_directory(
  validation_dir,
  validation_datagen,
  target_size = c(150,150),
  batch_size = 20,
  class_mode = "binary"
)

test_generator<-flow_images_from_directory(
  test_dir,
  test_datagen,
  target_size = c(150,150),
  batch_size=20,
  class_mode = "binary"
)


batch<-generator_next(train_generator)
str(batch)

history<-model %>% fit_generator(
  train_generator,
  steps_per_epoch = 20,
  epochs=20,
  validation_data=validation_generator,
  validation_steps = 20
)

setwd("Users/luzhang/Desktop/acc_loss")
model %>% save_model_hdf5("acc_loss_attempt01")
plot(history)

model %>% evaluate_generator(test_generator,steps=20)











