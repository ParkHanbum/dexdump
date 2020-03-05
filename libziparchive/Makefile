sdir := $(srcdir)/libziparchive
odir := $(objdir)/libziparchive

LIBZIP_SRCS := $(sdir)/zip_archive.cc
LIBZIP_SRCS += $(sdir)/zip_archive_stream_entry.cc
LIBZIP_SRCS += $(sdir)/zip_writer.cc

LIBZIP_OBJS := $(patsubst $(sdir)/%.cc,$(odir)/%.op,$(LIBZIP_SRCS))

_CXXFLAGS := $(CXXFLAGS)
_CXXFLAGS += -I$(ANDROID_HDRS) -I$(LIBLOG_HDRS) -I$(sdir)/include

all: $(odir)/libziparchive.op $(LIBZIP_OBJS)

$(odir)/%.op: $(sdir)/%.cc
	$(CXX) $(_CXXFLAGS) -c $< -o $@ 

$(odir)/libziparchive.op: $(LIBZIP_OBJS)
	$(LD) -r -o $@ $^

clean:
	rm -f $(odir)/*.op
